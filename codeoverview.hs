module CodeOverview( CodeDef
                   , ColorDef
                   , ViewColor

                   -- Defined languages
                   , cCodeDef
                   , haskellCodeDef
                   , ocamlCodeDef
                   , rubyCodeDef
                   , shellLikeCodeDef 
                   , htmlCodeDef
                   , emptyCodeDef 

                   -- Defined colorset
                   , defaultColorDef

                   -- Manipulation function
                   , createCodeOverview 
                   , addOverMask
                   , doubleSize
                   ) where

import Data.Char
import Data.Maybe( fromJust, isJust )
import Data.List( foldl', isPrefixOf )
--import System.IO.Unsafe

type ViewColor = (Int, Int, Int, Int)
newtype NextParse = NextParse ([ViewColor], Parser)
type ParseResult = Either NextParse (Maybe ([ViewColor], String))
type Parser = String -> ParseResult

data    CodeDef = CodeDef
    { lineComm         :: Maybe String
    , multiLineCommBeg :: Maybe String
    , multiLineCommEnd :: Maybe String
    , identParser :: Char -> Int -> Bool
    , strParser :: Maybe (ColorDef -> Parser)
    , tabSpace :: Int
    }

data    ColorDef = ColorDef
    { commentColor   :: ViewColor
    , stringColor    :: ViewColor
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
    , stringColor    = (100,100,155,255)
    , highlightColor = (200,200,100,255)
    , majColor       = (  0,  0,  0,255)
    , emptyColor     = (255,255,255,  0)
    , viewColor      = (200,200,255,255)
    }

cCodeDef, haskellCodeDef, ocamlCodeDef,
             rubyCodeDef, shellLikeCodeDef,
             htmlCodeDef, emptyCodeDef :: CodeDef

emptyCodeDef = CodeDef
            { lineComm = Nothing
            , multiLineCommBeg = Nothing
            , multiLineCommEnd = Nothing
            , tabSpace = 4
            , identParser = basicIdent
            , strParser = Nothing
            }

htmlCodeDef = emptyCodeDef
            { multiLineCommBeg = Just "<!--"
            , multiLineCommEnd = Just "-->"
            }

shellLikeCodeDef = emptyCodeDef
            { lineComm = Just "#"
            , tabSpace = 4
            , strParser = Just $ stringParser False shellLikeCodeDef
            }

rubyCodeDef = shellLikeCodeDef
    { multiLineCommBeg = Just "=begin"
    , multiLineCommEnd = Just "=end"
    }

cCodeDef = CodeDef
           { lineComm = Just "//"
           , multiLineCommBeg = Just "/*"
           , multiLineCommEnd = Just "*/"
           , tabSpace = 4
           , identParser = identWithPrime
           , strParser = Just $ stringParser False cCodeDef
           }

haskellCodeDef = CodeDef
                 { lineComm = Just "--"
                 , multiLineCommBeg = Just "{-"
                 , multiLineCommEnd = Just "-}"
                 , tabSpace = 4
                 , identParser = identWithPrime
                 , strParser = Just $ stringParser False haskellCodeDef
                 }

ocamlCodeDef = CodeDef
               { lineComm = Nothing
               , multiLineCommBeg = Just "(*"
               , multiLineCommEnd = Just "*)"
               , tabSpace = 4
               , identParser = basicIdent
               , strParser = Just $ stringParser False ocamlCodeDef
               }

identWithPrime :: Char -> Int -> Bool
identWithPrime c 0 = isAlpha c
identWithPrime c _ = isAlphaNum c || c == '\''

basicIdent :: Char -> Int -> Bool
basicIdent c 0 = isAlpha c
basicIdent c _ = isAlphaNum c

stringParser :: Bool -> CodeDef -> ColorDef -> Parser
stringParser allowBreak codeDef colorDef ('"':stringSuite) =
  stringer (color:) stringSuite
    where color = stringColor colorDef
          empty = emptyColor colorDef
          tabSize = tabSpace codeDef
          stringer acc [] = if allowBreak
                                then Left $ NextParse (acc [], stringer id)
                                else Right Nothing
          stringer acc ('\\':'"':xs) =
              stringer (acc . ([color, color]++)) xs
          stringer acc (' ':xs) = stringer (acc . (empty:)) xs
          stringer acc ('\t':xs) = stringer (acc . (replicate tabSize empty ++)) xs
          stringer acc ('"':xs) = Right $ Just (acc [color], xs)
          stringer acc (_:xs) = stringer (acc . (color:)) xs
stringParser _ _ _ _ = Right Nothing

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
      whenAdd (isJust $ lineComm codeDef) (monoLineComment codeDef colorDef)
    . whenAdd (isJust $ strParser codeDef) (fromJust (strParser codeDef) colorDef)
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

doubleSize :: [[ViewColor]] -> [[ViewColor]]
doubleSize = concatMap (\a -> [double a, double a])
    where double = concatMap $ \a -> [a,a]

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

createCodeOverview :: CodeDef -> ColorDef -> [String] -> [String] -> [[ViewColor]]
createCodeOverview codeDef colorDef highlighted =
        normalizePixelList colorDef
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

