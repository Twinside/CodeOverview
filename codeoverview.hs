module CodeOverview{-( CodeDef
                   , ColorDef
                   , ViewColor
                   )-} where

import Data.Char
import Data.List( foldl', isPrefixOf )
--import System.IO.Unsafe

type ViewColor = (Int, Int, Int, Int)
newtype NextParse = NextParse ([ViewColor], Parser)
type ParseResult = Either NextParse (Maybe ([ViewColor], String))
type Parser = String -> ParseResult

instance Show NextParse where
    show (NextParse (v, _)) = "NextParse (" ++ show v ++ ", _)"

data    CodeDef = CodeDef
    { lineComm         :: Maybe String
    , multiLineCommBeg :: Maybe String
    , multiLineCommEnd :: Maybe String
    , identParser :: Char -> Int -> Bool
    --, strParser :: Char -> Char -> Bool
    , tabSpace :: Int
    }

data    ColorDef = ColorDef
    { commentColor   :: ViewColor
    , normalColor    :: ViewColor
    , highlightColor :: ViewColor
    , majColor       :: ViewColor
    , emptyColor     :: ViewColor
    , viewColor      :: ViewColor
    }

defaultColorDef :: ColorDef
defaultColorDef = ColorDef
    { commentColor   = (100,155,100,255)
    , normalColor    = (128,128,128,255)
    , highlightColor = (200,200,100,255)
    , majColor       = (  0,  0,  0,255)
    , emptyColor     = (255,255,255,  0)
    , viewColor      = (200,200,255,255)
    }

cCodeDef, haskellCodeDef, ocamlCodeDef :: CodeDef
cCodeDef = CodeDef
           { lineComm = Just "//"
           , multiLineCommBeg = Just "/*"
           , multiLineCommEnd = Just "*/"
           , tabSpace = 4
           , identParser = identWithPrime
           }

haskellCodeDef = CodeDef
                 { lineComm = Just "--"
                 , multiLineCommBeg = Just "{-"
                 , multiLineCommEnd = Just "-}"
                 , tabSpace = 4
                 , identParser = identWithPrime
                 }

ocamlCodeDef = CodeDef
               { lineComm = Nothing
               , multiLineCommBeg = Just "(*"
               , multiLineCommEnd = Just "*)"
               , tabSpace = 4
               , identParser = basicIdent
               }

identWithPrime :: Char -> Int -> Bool
identWithPrime c 0 = isAlpha c
identWithPrime c _ = isAlphaNum c || c == '\''

basicIdent :: Char -> Int -> Bool
basicIdent c 0 = isAlpha c
basicIdent c _ = isAlphaNum c

monoLineComment :: CodeDef -> ColorDef -> Parser
monoLineComment cdef colors toMatch 
  | initial `isPrefixOf` toMatch = Right $ Just (map (const color) toMatch, "")
  | otherwise = Right Nothing
    where color = commentColor colors
          (Just initial) = lineComm cdef

multiLineComment :: CodeDef -> ColorDef -> Parser
multiLineComment cdef colors toMatch
  | initial `isPrefixOf` toMatch = multiParse initSize 1 $ drop initSize toMatch
  | otherwise = Right Nothing
    where color = commentColor colors
          Just initial = multiLineCommBeg cdef
          initSize = length initial

          Just end = multiLineCommEnd cdef
          endSize = length end

          multiParse :: Int -> Int -> Parser
          multiParse acc level [] =
                Left $ NextParse (replicate acc color, multiParse 0 level)

          multiParse acc level x@(_:xs)
            | initial `isPrefixOf` x =
                multiParse (acc + initSize) (level + 1) $ drop initSize x

            | end `isPrefixOf` x && level - 1 == 0 =
                Right $ Just (replicate (acc + endSize) color, drop endSize x)

            | end `isPrefixOf` x =
                multiParse (acc + endSize) (level - 1) $ drop endSize x
                
            | otherwise = multiParse (acc+1) level xs

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

                  prepareWord w | w `elem` highlightList = replicate (length w) colorHi
                                | otherwise = map (\a -> if isUpper a then colorMaj else colorNormal) w

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
      whenAdd (lineComm codeDef /= Nothing) (monoLineComment codeDef colorDef)
    . whenAdd (multiLineCommBeg codeDef /= Nothing
              && multiLineCommEnd codeDef /= Nothing) (multiLineComment codeDef colorDef)
    $ [ globalParse highlightDef codeDef colorDef
      , charEater codeDef colorDef
      ]

normalizePixelList :: ColorDef -> [[ViewColor]] -> [[ViewColor]]
normalizePixelList colorDef lst = map normalize $ zip lst sizes
    where sizes = map length lst
          maxi = maximum sizes
          color = emptyColor colorDef
          padding = maxi `mod` 4
          normalize (line, size) =
              line ++ replicate (maxi - size + padding) color

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

createCodeOverview :: CodeDef -> ColorDef -> [String] -> [[ViewColor]]
createCodeOverview codeDef colorDef = normalizePixelList colorDef
                                    . (\f -> f [])
                                    . fst
                                    . foldl' parse (id, Nothing)
    where usedParser = parserList ["lineEval"] codeDef colorDef
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

