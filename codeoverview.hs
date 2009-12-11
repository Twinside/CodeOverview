module CodeOverview{-( CodeDef
                   , ColorDef
                   , ViewColor
                   )-} where

import Data.Char

type ViewColor = (Int, Int, Int, Int)
newtype NextParse = NextParse ([ViewColor], Parser)
type Parser = String -> Either NextParse (Maybe ([ViewColor], String))

data    CodeDef = CodeDef
    { lineComm         :: Maybe String
    , multiLineCommBeg :: Maybe String
    , multiLineCommEnd :: Maybe String
    , identParser :: Char -> Int -> Bool
    , tabSpace :: Int
    }

data    ColorDef = ColorDef
    { commentColor   :: ViewColor
    , normalColor    :: ViewColor
    , highlightColor :: ViewColor
    , majColor       :: ViewColor
    , emptyColor     :: ViewColor
    }

cCodeDef, haskellCodeDef, ocamlCodeDef :: CodeDef
cCodeDef = CodeDef
           { lineComm = Just "//"
           , multiLineCommBeg = Just "/*"
           , multiLineCommEnd = Just "*/"
           }

haskellCodeDef = CodeDef
                 { lineComm = Just "--"
                 , multiLineCommBeg = Just "{-*"
                 , multiLineCommEnd = Just "-}"
                 }

ocamlCodeDef = CodeDef
               { lineComm = Nothing
               , multiLineCommBeg = Just "(*"
               , multiLineCommEnd = Just "*)"
               }

headMatch :: String -> String -> Bool
headMatch a b = all (uncurry (==)) $ zip a b

monoLineComment :: CodeDef -> ColorDef -> Parser
monoLineComment cdef colors toMatch 
  | headMatch init toMatch = Right $ Just (map (const color) toMatch, "")
  | otherwise = Right Nothing
    where color = commentColor colors
          (Just init) = lineComm cdef

multiLineComment :: CodeDef -> ColorDef -> Parser
multiLineComment cdef colors toMatch
  | headMatch init toMatch = multiParse initSize 1 toMatch
  | otherwise = Right Nothing
    where color = commentColor colors
          Just init = multiLineCommBeg cdef
          initSize = length init

          Just end = multiLineCommEnd cdef
          endSize = length end

          multiParse :: Int -> Int -> Parser
          multiParse acc level [] = Left
                                  $ NextParse (replicate acc color, multiParse 0 level)
          multiParse acc level x@(_:xs)
            | headMatch init x =
                multiParse (acc + initSize) (level+1) $ drop initSize x

            | headMatch end x && level == 1 =
                Right $ Just (replicate (acc + endSize) color, drop endSize x)

            | headMatch end x =
                multiParse (acc + endSize) (level-1) $ drop endSize x
                
            | otherwise = multiParse (acc+1) level xs

eatTillSpace :: (Char -> Int -> Bool) -> String -> (String, String)
eatTillSpace f = eater 0
    where eater _ [] = ([], [])
          eater _ l@(' ':xs) = ([], l)
          eater _ l@('\t':xs) = ([], l)
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

createCodeOverview :: CodeDef -> ColorDef -> [String] -> [[ViewColor]]
createCodeOverview codeDef colorDef lst = foldl' parse ([], Nothing)
    where usedParser = parserList [] codeDef colorDef
          parse (prevLines, Just parse) line = lineEval (parse line)
          parse (prevLines, Nothing) line =

          lineEval (Left (NextParse (vals, parser))) = (vals, Just parser)
          lineEval (Right Nothing) (parser: subParser) = lineEval (parser ??) subParser
          lineEval (Right (Just (chars, rest))) =

