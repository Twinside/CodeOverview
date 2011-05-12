{-# LANGUAGE ViewPatterns #-}
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

import Control.Monad.State

import Data.Char
import Data.Maybe( fromJust, isJust )
import Data.List( mapAccumL, sortBy )
import qualified Data.Map as Map
import CodeOverviewGenerator.ByteString(uncons)
import qualified CodeOverviewGenerator.ByteString as B

import CodeOverviewGenerator.Color
import CodeOverviewGenerator.Language

--------------------------------------------------
----            Generation code
--------------------------------------------------

-- | Parse a commentary from a beginning marker till the
-- end of the line.
monoLineComment :: CodeDef [ViewColor] -> ColorDef -> Parser [ViewColor]
monoLineComment cdef colors = Parser innerMatch
    where innerMatch toMatch
             | initial `B.isPrefixOf` toMatch = 
                    return $ Result (concatMap colorer $ B.unpack toMatch, B.empty)
             | otherwise = return NoParse
            
          color = commentColor colors
          eColor = emptyColor colors
          (Just initial) = lineComm cdef

          colorer ' ' = [eColor]
          colorer '\t' = replicate (tabSpace cdef) eColor
          colorer _ = [color]


-- | Parse multiline comments.
-- Comments can be nested, the parsing is recursive.
multiLineComment :: CodeDef [ViewColor] -> ColorDef -> Parser [ViewColor]
multiLineComment cdef colors = Parser $ innerParser 
  where innerParser toMatch
          | initial `B.isPrefixOf` toMatch = 
                multiParse (replicate initSize color ++) (1 :: Int)
                                        $ B.drop initSize toMatch
          | otherwise = return NoParse
        
        color = commentColor colors
        eColor = emptyColor colors
        Just initial = multiLineCommBeg cdef
        initSize = B.length initial

        Just end = multiLineCommEnd cdef
        endSize = B.length end

        multiParse acc level (uncons -> Nothing) =
          return $ NextParse (acc [], Parser $ multiParse id level)

        multiParse acc level (uncons -> Just (' ',xs)) =
            multiParse (acc . (eColor:)) level xs
        multiParse acc level (uncons -> Just ('\t',xs)) =
            multiParse (acc . (replicate (tabSpace cdef) eColor ++)) level xs

        multiParse acc level x@(uncons -> Just (_,xs))
          | initial `B.isPrefixOf` x =
              multiParse (acc . (replicate initSize color++)) (level + 1)
                         $ B.drop initSize x

          | end `B.isPrefixOf` x && level - 1 == 0 =
              return $ Result (acc $ replicate endSize color, B.drop endSize x)

          | end `B.isPrefixOf` x =
              multiParse (acc . (replicate endSize color++)) (level - 1)
                      $ B.drop endSize x

          | otherwise = multiParse (acc . (color:)) level xs
        multiParse _ _ _ = error "Compilator pleaser... multiParse"

-- | Given a tokenizer and a string, cut a string in a token
-- and a rest.
eatTillSpace :: (Char -> Int -> Bool) -> B.ByteString -> (B.ByteString, B.ByteString)
eatTillSpace f = eater 0
    where eater _ (uncons -> Nothing) = (B.empty, B.empty)
          eater _ l@(uncons -> Just (' ',_)) = (B.empty, l)
          eater _ l@(uncons -> Just ('\t',_)) = (B.empty, l)
          eater i l@(uncons -> Just (a,xs))
            | f a i = (\(wordEnd, rest) -> (a `B.cons` wordEnd, rest)) $ eater (i+1) xs
            | otherwise = (B.empty, l)
          eater _ _ = error "Compilator pleaser eatTillSpace"

globalParse :: [String] -> CodeDef [ViewColor] -> ColorDef -> Parser [ViewColor]
globalParse highlightList codeDef colorDef = Parser $ \toParse ->
    let (word, rest) = eatTillSpace (identParser codeDef) toParse
    in if B.null word
          then return NoParse
          else return $ Result (prepareWord word, rest)
            where colorHi = highlightColor colorDef
                  colorMaj = majColor colorDef
                  colorNormal = normalColor colorDef

                  highlightByteList = map B.pack highlightList

                  prepareWord w
                    | w `elem` highlightByteList = replicate (B.length w) colorHi
                    | otherwise = case w `Map.lookup` specialIdentifier codeDef of
                        Nothing -> map (\a -> if isUpper a then colorMaj else colorNormal)
                                    $ B.unpack w
                        Just c -> replicate (B.length w) c

charEater :: CodeDef [ViewColor] -> ColorDef -> Parser [ViewColor]
charEater codeDef colorDef = Parser inner
  where inner (uncons -> Nothing) = return NoParse
        inner (uncons -> Just ('\t',xs)) =
            return $ Result (replicate size color, xs)
                where size = tabSpace codeDef
                      color = emptyColor colorDef
        inner (uncons -> Just (' ',xs)) =
            return $ Result ([emptyColor colorDef], xs)
        inner (uncons -> Just ( _ ,xs)) =
            return $ Result ([normalColor colorDef], xs)
        inner _ = error "Compiler pleaser charEater"

whenAdd :: Bool -> a -> [a] -> [a]
whenAdd yesno a = if yesno then (a:) else id

parserList :: [String] -> CodeDef [ViewColor] -> ColorDef -> [Parser [ViewColor]]
parserList highlightDef codeDef colorDef =
      (specificParser codeDef ++)
    . (charParser colorDef:)
    . whenAdd (isJust $ lineComm codeDef) (monoLineComment codeDef colorDef)
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
createCodeOverview :: CodeDef [ViewColor] -- ^ Language definition used to put some highlight/color
                   -> ColorDef       -- ^ Colors to be used during the process.
                   -> [(String,Int)] -- ^ Error line definition, to put an highlight on some lines.
                   -> [String]       -- ^ Identifier to be 'highlighted', to highlight a search
                   -> [B.ByteString]       -- ^  The lines from the file
                   -> ([[ViewColor]], ColoringContext)
createCodeOverview codeDef colorDef errorLines highlighted file =
    runState (createCodeOverview' codeDef colorDef errorLines highlighted file)
             defaultColoringContext 

createCodeOverview' :: CodeDef [ViewColor] -- ^ Language definition used to put some highlight/color
                   -> ColorDef       -- ^ Colors to be used during the process.
                   -> [(String,Int)] -- ^ Error line definition, to put an highlight on some lines.
                   -> [String]       -- ^ Identifier to be 'highlighted', to highlight a search
                   -> [B.ByteString]       -- ^  The lines from the file
                   -> State ColoringContext [[ViewColor]]
createCodeOverview' codeDef colorDef errorLines highlighted file = do
        (parseRez, _) <- foldM parse (id, Nothing) file
        return . addOverLines colorDef errorLines 
               . normalizePixelList colorDef
               $ parseRez []

    where usedParser = parserList highlighted codeDef colorDef
          (Parser firstParser : tailParser) = usedParser

          parse (prevLines, Just (Parser parser)) line = do
              lineRest <- parser line
              (line', parser') <- lineEval (id, line, lineRest) usedParser
              return  (prevLines . (line':), parser')

          parse (prevLines, Nothing) line = do
              parsedLine <- firstParser line 
              (line', parser') <- lineEval (id, line, parsedLine) 
                                          tailParser
              return (prevLines . (line':), parser')

          lineEval (line,      _, NextParse (vals, parser)) _ =
              return (line vals, Just parser)
          lineEval (line,      _, Result (chars, (uncons -> Nothing))) _ =
              return (line chars, Nothing)
          lineEval (line,(uncons -> Nothing), NoParse) _ =
              return (line [], Nothing)

          lineEval (line, parsed, NoParse) (Parser parser: subParser) = do
              neoParsed <- parser parsed
              lineEval (line, parsed, neoParsed) subParser

          lineEval (line,      _, Result (chars, rest))      _ = do
              rez <- firstParser rest 
              let neoLine = line . (chars ++)
              lineEval (neoLine, rest, rez) tailParser

          lineEval                                    _ [] =
              error "Unable to parse, shouldn't happen"

