module CodeOverviewGenerator.CodeOverview ( 
                   -- * Types 
                     CodeDef
                   , ColorDef
                   , ViewColor

                   -- * Colorsets
                   , defaultColorDef
                   , parseColorDef

                   -- * Manipulation function
                   , createCodeOverview 
                   , addOverMask
                   , doubleSize
                   ) where

import Data.Char
import Data.Maybe( fromJust, isJust )
import Data.List( foldl', isPrefixOf, mapAccumL, sortBy )
import qualified Data.Map as Map

import CodeOverviewGenerator.Color
import CodeOverviewGenerator.Language

--------------------------------------------------
----            Generation code
--------------------------------------------------

-- | Parse a commentary from a beginning marker till the
-- end of the line.
monoLineComment :: CodeDef -> ColorDef -> Parser
monoLineComment cdef colors toMatch 
  | initial `isPrefixOf` toMatch = Right $ Just (concat $ map colorer toMatch, "")
  | otherwise = Right Nothing
    where color = commentColor colors
          eColor = emptyColor colors
          (Just initial) = lineComm cdef

          colorer ' ' = [eColor]
          colorer '\t' = replicate (tabSpace cdef) eColor
          colorer _ = [color]


-- | Parse multiline comments.
-- Comments can be nested, the parsing is recursive.
multiLineComment :: CodeDef -> ColorDef -> Parser
multiLineComment cdef colors toMatch
  | initial `isPrefixOf` toMatch = multiParse (replicate initSize color ++) 1
                                 $ drop initSize toMatch
  | otherwise = Right Nothing
    where color = commentColor colors
          eColor = emptyColor colors
          Just initial = multiLineCommBeg cdef
          initSize = length initial

          Just end = multiLineCommEnd cdef
          endSize = length end

          multiParse :: ([ViewColor] -> [ViewColor]) -> Int -> Parser
          multiParse acc level [] =
                Left $ NextParse (acc [], multiParse id level)

          multiParse acc level (' ':xs) =
              multiParse (acc . (eColor:)) level xs
          multiParse acc level ('\t':xs) =
              multiParse (acc . (replicate (tabSpace cdef) eColor ++)) level xs

          multiParse acc level x@(_:xs)
            | initial `isPrefixOf` x =
                multiParse (acc . (replicate initSize color++)) (level + 1)
                           $ drop initSize x

            | end `isPrefixOf` x && level - 1 == 0 =
                Right $ Just (acc $ replicate endSize color, drop endSize x)

            | end `isPrefixOf` x =
                multiParse (acc . (replicate endSize color++)) (level - 1) $ drop endSize x
                
            | otherwise = multiParse (acc . (color:)) level xs

-- | Given a tokenizer and a string, cut a string in a token
-- and a rest.
eatTillSpace :: (Char -> Int -> Bool) -> String -> (String, String)
eatTillSpace f = eater 0
    where eater _ [] = ([], [])
          eater _ l@(' ':_) = ([], l)
          eater _ l@('\t':_) = ([], l)
          eater i (a:xs) 
            | f a i = (\(wordEnd, rest) -> (a:wordEnd, rest)) $ eater (i+1) xs
            | otherwise = ([],a:xs)

globalParse :: [String] -> CodeDef -> ColorDef -> Parser
globalParse highlightList codeDef colorDef toParse =
    case eatTillSpace (identParser codeDef) toParse of
        ([],    _)   -> Right Nothing
        (word, rest) -> Right $ Just (prepareWord word, rest)
            where colorHi = highlightColor colorDef
                  colorMaj = majColor colorDef
                  colorNormal = normalColor colorDef

                  prepareWord w 
                    | w `elem` highlightList = replicate (length w) colorHi
                    | otherwise = case w `Map.lookup` specialIdentifier codeDef of
                    Nothing -> map (\a -> if isUpper a then colorMaj else colorNormal) w
                    Just c -> replicate (length w) c

charEater :: CodeDef -> ColorDef -> Parser
charEater       _        _        [] = Right Nothing
charEater codeDef colorDef ('\t':xs) = Right $ Just (replicate size color, xs)
    where size = tabSpace codeDef
          color = emptyColor colorDef
charEater        _ colorDef (' ':xs) = Right $ Just ([emptyColor colorDef], xs)
charEater        _ colorDef ( _ :xs) = Right $ Just ([normalColor colorDef], xs)

whenAdd :: Bool -> a -> [a] -> [a]
whenAdd yesno a = if yesno then (a:) else id

parserList :: [String] -> CodeDef -> ColorDef -> [Parser]    
parserList highlightDef codeDef colorDef =
      whenAdd (isJust $ lineComm codeDef) (monoLineComment codeDef colorDef)
    . whenAdd (isJust $ strParser codeDef) (fromJust (strParser codeDef) colorDef)
    . whenAdd (multiLineCommBeg codeDef /= Nothing
              && multiLineCommEnd codeDef /= Nothing) (multiLineComment codeDef colorDef)
    $ [ globalParse highlightDef codeDef colorDef
      , charEater codeDef colorDef
      ]

-- | Make all the lines to the same length, fill the void
-- with emptyColor
normalizePixelList :: ColorDef -> [[ViewColor]] -> [[ViewColor]]
normalizePixelList colorDef lst = map normalize $ zip lst sizes
    where sizes = map length lst
          maxi = maximum sizes
          color = emptyColor colorDef
          padding = maxi `mod` 4
          normalize (line, size) =
              line ++ replicate (maxi - size + padding) color

doubleSize :: [[ViewColor]] -> [[ViewColor]]
doubleSize = concatMap (\a -> [double a, double a])
    where double = concatMap $ \a -> [a,a]

alphaBlend :: ViewColor -> ViewColor -> ViewColor
alphaBlend  (r, g, b, a) (r', g', b', a') = (rf, gf, bf, af)
    where coef = 255 - a'
          rf = (r * coef + r' * a') `quot` 256
          gf = (g * coef + g' * a') `quot` 256
          bf = (b * coef + b' * a') `quot` 256
          af = (a * coef + a' * a') `quot` 256

-- | Draw a rectangle with alpha blending over the generated image.
addOverMask :: ColorDef -> (Int, Int) -> (Int, Int) -> [[ViewColor]]
            -> [[ViewColor]]
addOverMask colorDef (x,y) (width, height) pixels = prelude ++ map lineColoration toTreat ++ end
    where (prelude, secondPart) = splitAt y pixels
          (toTreat, end) = splitAt height secondPart
          (rv, gv, bv, av) = viewColor colorDef

          lineColoration line = 
            let (prefix, endOf) = splitAt x line
                (pixelToTreat, lastPart) = splitAt width endOf
            in prefix ++ map pixelUpdater pixelToTreat ++ lastPart

          pixelUpdater (r, g, b, a) =
              ( (r * rv) `quot` 256
              , (g * gv) `quot` 256
              , (b * bv) `quot` 256
              , (a * av) `quot` 256
              )

-- | Add error layer on top of a generated image.
-- Work best if all lines are of the same length
addOverLines :: ColorDef -> [(String, Int)] -> [[ViewColor]] -> [[ViewColor]]
addOverLines _ [] = id
addOverLines colordef errorList = snd . mapAccumL lineMarker sortedErrors . zip [1..]
  where sortedErrors = sortBy (\(_,line) (_,line') -> compare line line') errorList
        lineMarker [] (_, e) = ([], emptyConcat ++ e)
        lineMarker fullList@((hiKind, lineNumber):xs) element@(currLine, e)
          | lineNumber < currLine = lineMarker xs element
          | lineNumber > currLine = (fullList, emptyConcat ++ e)
          -- lineNumber == currLine
          | otherwise = (xs, highlightLine hiKind e)

        errorColor = errorLineColor colordef
        warningColor = warningLineColor colordef
        infoColor = infoLineColor colordef
        marginColor = highlightColor colordef

        errorLeftMargin = 15

        emptyConcat = replicate errorLeftMargin (emptyColor colordef) ++ [marginColor]
        infoConcat = replicate errorLeftMargin infoColor ++ [marginColor]
        warningConcat =  replicate errorLeftMargin warningColor ++ [marginColor]
        errorConcat =  replicate errorLeftMargin errorColor ++ [marginColor]

        highlightLine ('i':_) line = infoConcat ++ map (\a -> alphaBlend a infoColor) line
        highlightLine ('I':_) line = infoConcat ++ map (\a -> alphaBlend a infoColor) line
        highlightLine ('w':_) line = warningConcat ++ map (\a -> alphaBlend a warningColor) line
        highlightLine ('W':_) line = warningConcat ++ map (\a -> alphaBlend a warningColor) line
        highlightLine _ line = errorConcat ++ map (\a -> alphaBlend a errorColor) line

-- | Main function to create an overview of a parsed file
createCodeOverview :: CodeDef        -- ^ Language definition used to put some highlight/color
                   -> ColorDef       -- ^ Colors to be used during the process.
                   -> [(String,Int)] -- ^ Error line definition, to put an highlight on some lines.
                   -> [String]       -- ^ Identifier to be 'highlighted', to highlight a search
                   -> [String]       -- ^  The lines from the file
                   -> [[ViewColor]]
createCodeOverview codeDef colorDef errorLines highlighted =
        addOverLines colorDef errorLines
      . normalizePixelList colorDef
      . (\f -> f [])
      . fst
      . foldl' parse (id, Nothing)
    where usedParser = parserList highlighted codeDef colorDef
          (firstParser : tailParser) = usedParser

          parse (prevLines, Just parser) line =  (prevLines . (line':), parser')
                where (line', parser') = lineEval (id, line, parser line) usedParser
          parse (prevLines, Nothing) line = (prevLines . (line':), parser')
                where (line', parser') = lineEval (id, line, firstParser line) tailParser

          lineEval :: ([ViewColor] -> [ViewColor], String, ParseResult) -> [Parser]
                   -> ([ViewColor], Maybe Parser)
          lineEval (line,      _, Left (NextParse (vals, parser))) _ = (line vals, Just parser)
          lineEval (line,      _, Right (Just (chars, [])))        _ = (line chars, Nothing)
          lineEval (line,     [], Right Nothing)                   _ = (line [], Nothing)

          lineEval (line, parsed, Right Nothing) (parser: subParser) =
              lineEval (line, parsed, parser parsed) subParser

          lineEval (line,      _, Right (Just (chars, rest)))      _ =
              lineEval (neoLine, rest, firstParser rest) tailParser
                    where neoLine = line . (chars ++)
          lineEval                                    _ [] =
              error "Unable to parse, shouldn't happen"

