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
                   , createHeatMap
                   , addOverMask
                   , doubleSize
                   ) where

import Control.Applicative
import Control.Monad.State

import Data.Array
import Data.Char
import Data.Maybe( fromJust, isJust )
import Data.List( mapAccumL, sortBy, transpose,
                            sort, maximumBy, group )

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
monoLineComment :: CodeDef [CodeEntity] -> Parser [CodeEntity]
monoLineComment cdef = Parser innerMatch
    where innerMatch toMatch
             | initial `B.isPrefixOf` toMatch = 
                    return $ Result (concatMap colorer $ B.unpack toMatch, B.empty)
             | otherwise = return NoParse
            
          (Just initial) = lineComm cdef

          colorer ' ' = [EmptyEntity]
          colorer '\t' = replicate (tabSpace cdef) EmptyEntity
          colorer _ = [CommentEntity]


-- | Parse multiline comments.
-- Comments can be nested, the parsing is recursive.
multiLineComment :: CodeDef [CodeEntity] -> Parser [CodeEntity]
multiLineComment cdef = Parser $ innerParser 
  where innerParser toMatch
          | initial `B.isPrefixOf` toMatch = 
                multiParse (replicate initSize CommentEntity ++) (1 :: Int)
                                        $ B.drop initSize toMatch
          | otherwise = return NoParse
        recurse = recursiveComment cdef
        Just initial = multiLineCommBeg cdef
        initSize = B.length initial

        Just end = multiLineCommEnd cdef
        endSize = B.length end

        multiParse acc level (uncons -> Nothing) =
          return $ NextParse (acc [], Parser $ multiParse id level)

        multiParse acc level (uncons -> Just (' ',xs)) =
            multiParse (acc . (EmptyEntity:)) level xs
        multiParse acc level (uncons -> Just ('\t',xs)) =
            multiParse (acc . (replicate (tabSpace cdef) EmptyEntity ++)) level xs

        multiParse acc level x@(uncons -> Just (_,xs))
          | initial `B.isPrefixOf` x && recurse =
              multiParse (acc . (replicate initSize CommentEntity++)) (level + 1)
                         $ B.drop initSize x

          | end `B.isPrefixOf` x && level - 1 == 0 =
              return $ Result (acc $ replicate endSize CommentEntity,
                               B.drop endSize x)

          | end `B.isPrefixOf` x =
              multiParse (acc . (replicate endSize CommentEntity++)) (level - 1)
                      $ B.drop endSize x

          | otherwise = multiParse (acc . (CommentEntity:)) level xs
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

globalParse :: [String] -> CodeDef [CodeEntity] -> Parser [CodeEntity]
globalParse highlightList codeDef = Parser $ \toParse ->
    let (word, rest) = eatTillSpace (identParser codeDef) toParse
    in if B.null word
          then return NoParse
          else return $ Result (prepareWord word, rest)
            where highlightByteList = map B.pack highlightList

                  prepareWord w
                    | w `elem` highlightByteList = replicate (B.length w) HighlightEntity
                    | otherwise = case w `Map.lookup` specialIdentifier codeDef of
                        Nothing -> map (\a -> if isUpper a then MajEntity else NormalEntity)
                                    $ B.unpack w
                        Just c -> replicate (B.length w) c

charEater :: CodeDef [a] -> Parser [CodeEntity]
charEater codeDef = Parser inner
  where inner (uncons -> Nothing) = return NoParse
        inner (uncons -> Just ('\t',xs)) =
            return $ Result (replicate size EmptyEntity, xs)
                where size = tabSpace codeDef
        inner (uncons -> Just (' ',xs)) =
            return $ Result ([EmptyEntity], xs)
        inner (uncons -> Just ( _ ,xs)) =
            return $ Result ([NormalEntity], xs)
        inner _ = error "Compiler pleaser charEater"

whenAdd :: Bool -> a -> [a] -> [a]
whenAdd yesno a = if yesno then (a:) else id

parserList :: [String] -> CodeDef [CodeEntity] -> [Parser [CodeEntity]]
parserList highlightDef codeDef =
      (specificParser codeDef ++)
    . (charParser :)
    . whenAdd (isJust $ strParser codeDef) (fromJust (strParser codeDef))
    . whenAdd (isJust $ lineComm codeDef) (monoLineComment codeDef)
    . whenAdd (multiLineCommBeg codeDef /= Nothing
              && multiLineCommEnd codeDef /= Nothing) 
                (multiLineComment codeDef)
    $ [ globalParse highlightDef codeDef
      , charEater codeDef
      ]

-- | Make all the lines to the same length, fill the void
-- with the first argument
normalizePixelList :: a -> [[a]] -> [[a]]
normalizePixelList fillValue lst = map normalize $ zip lst sizes
    where sizes = map length lst
          maxi = maximum sizes
          padding = maxi `mod` 4
          normalize (line, size) =
              line ++ replicate (maxi - size + padding) fillValue

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


createHeatMap :: CodeDef a
              -> ColorDef
              -> [(String,Int)]
              -> [B.ByteString]
              -> [[ViewColor]]
createHeatMap codeDef colorDef errorLines fileLines =
        let (parseRez, _) = evalState (foldM parse (id, 0) fileLines) defaultColoringContext
        in addOverLines colorDef errorLines 
               . normalizePixelList (emptyColor colorDef)
               $ parseRez []
    where langParser = foldl1 (<|>) (heatTokens codeDef)
                    <|> (return (1, 0) <$> anyChar)

          colorOfDepth i = colorRamp ! realIndex
            where colorRamp = heatRamp colorDef
                  (mini, maxi) = bounds colorRamp
                  realIndex = min (max mini i) maxi

          parse (prevLines, depth) line = do
              (line', depth') <- parseLine (id, depth) line
              return (prevLines . (line':), depth')

          parseLine :: ([ViewColor] -> [ViewColor], Int) -> B.ByteString 
                    -> State ColoringContext ([ViewColor], Int)
          parseLine (line, depth) (uncons -> Nothing) = return (line [], depth)
          parseLine (line, depth) toParse = do
              parseResult <- runParse langParser toParse
              case parseResult of
                NoParse -> return (line [], depth)
                NextParse ((count, increase), _) ->
                    let newColor = replicate count (colorOfDepth depth)
                    in return (line newColor, depth + increase)

                Result ((count, increase), rest) ->
                    let newColor = replicate count (colorOfDepth depth)
                    in parseLine (line . (newColor ++), depth + increase) rest

-- | Main function to create an overview of a parsed file
createCodeOverview :: CodeDef [CodeEntity] -- ^ Language definition used to put some highlight/color
                   -> ColorDef       -- ^ Colors to be used during the process.
                   -> [(String,Int)] -- ^ Error line definition, to put an highlight on some lines.
                   -> [String]       -- ^ Identifier to be 'highlighted', to highlight a search
                   -> [B.ByteString]       -- ^  The lines from the file
                   -> ([[ViewColor]], ColoringContext)
createCodeOverview codeDef colorDef errorLines highlighted file =
  ( addOverLines colorDef errorLines colorImage, ctxt)
    where (img, ctxt) = runState (createCodeOverview' codeDef highlighted file)
                         defaultColoringContext 
          colorImage = [ map (colorAssoc !) imgLine | imgLine <- img ]
          colorAssoc = makeEntityColorLookupTable colorDef


createCodeOverview' :: CodeDef [CodeEntity] -- ^ Language definition used to put some highlight/color
                    -> [String]       -- ^ Identifier to be 'highlighted', to highlight a search
                    -> [B.ByteString]       -- ^  The lines from the file
                    -> State ColoringContext [[CodeEntity]]
createCodeOverview' codeDef highlighted file = do
        (parseRez, _) <- foldM parse (id, Nothing) file
        return . normalizePixelList EmptyEntity
               $ parseRez []

    where Parser wholeParser = foldl1 (<|>) $ parserList highlighted codeDef

          parse (prevLines, Just (Parser parser)) line = do
              lineRest <- parser line
              (line', parser') <- lineEval (id, line, lineRest)
              return  (prevLines . (line':), parser')

          parse (prevLines, Nothing) line = do
              parsedLine <- wholeParser line 
              (line', parser') <- lineEval (id, line, parsedLine) 
              return (prevLines . (line':), parser')

          lineEval (line,      _, NextParse (vals, parser)) =
              return (line vals, Just parser)
          lineEval (line,      _, Result (chars, (uncons -> Nothing))) =
              return (line chars, Nothing)
          lineEval (line,(uncons -> Nothing), NoParse) =
              return (line [], Nothing)

          lineEval (line, parsed, NoParse) = do
              neoParsed <- wholeParser parsed
              lineEval (line, parsed, neoParsed)

          lineEval (line,      _, Result (chars, rest)) = do
              rez <- wholeParser rest 
              let neoLine = line . (chars ++)
              lineEval (neoLine, rest, rez)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n lst = rez : splitEvery n rest
    where (rez, rest) = splitAt (n-1) lst

-- | Reduce an image of an integer factor n, each element
-- in the resulting matrix will be the most present element
-- in a submatrix of size n*n.
-- todo : an array version
reducePixelMatrix :: (Ord a) => Int -> [[a]] -> [[a]]
reducePixelMatrix n lineList =
  [ map (valueOfGroup . kindsInBlock) $ blockList lineGroup
            | lineGroup <- splitEvery n lineList ]
    where -- Split each line in a group of chunk, and
          -- then create a line representing a sub matrix
          -- within lineList
          blockList = transpose . map (splitEvery n)

          -- for each submatrix, create a list of selement,
          -- and prepare it for counting
          kindsInBlock = group . sort . concat

          -- We want the last value with the most element in it
          valueOfGroup = fst 
                       . maximumBy (\(_,a) (_,b) -> compare a b)
                       . map countSplit 

          countSplit (x:xs) = (x, 1 + length xs)
          countSplit [] = error "Unhandled case - reducePixelMatrix.countSplit"



