module CodeOverview(-- * Types 
                     CodeDef
                   , ColorDef
                   , ViewColor

                   -- * Defined languages
                   , cCodeDef
                   , haskellCodeDef
                   , ocamlCodeDef
                   , rubyCodeDef
                   , shellLikeCodeDef
                   , pythonCodeDef
                   , htmlCodeDef
                   , emptyCodeDef

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
import qualified Data.Set as Set

--------------------------------------------------
----            Generation types
--------------------------------------------------
type ViewColor = (Int, Int, Int, Int)
newtype NextParse = NextParse ([ViewColor], Parser)
type ParseResult = Either NextParse (Maybe ([ViewColor], String))
type Parser = String -> ParseResult

-- | Define a language used by the image generator to put
-- some colors in it.
data    CodeDef = CodeDef
    { -- | Beginning marker for mono line market
      lineComm         :: Maybe String  
      -- | Beginning marker for multilines comments, like
      -- \'/*\' in C or \'{-\' in Haskell
    , multiLineCommBeg :: Maybe String
      -- | End  marker for multiline comments, like
      -- \'*/\' in C or \'-}\' in Haskell
    , multiLineCommEnd :: Maybe String
      -- | Definition for identifier for the current language.
    , identParser :: Char -> Int -> Bool
      -- | Definition for strings in the current language.
    , strParser :: Maybe (ColorDef -> Parser)
      -- | How we must transform tab into space.
    , tabSpace :: Int
      -- | Keyword list, for better coloration.
    , keywordList :: Set.Set String
      -- | Type list, for better coloration.
    , typeList :: Set.Set String
    }

-- | Color configuration for image generation.
data    ColorDef = ColorDef
    { commentColor   :: ViewColor
    , stringColor    :: ViewColor
    , normalColor    :: ViewColor
    , highlightColor :: ViewColor
    , majColor       :: ViewColor
    , emptyColor     :: ViewColor
    , viewColor      :: ViewColor
    , keywordColor   :: ViewColor
    , typeColor      :: ViewColor

    , errorLineColor :: ViewColor
    , warningLineColor :: ViewColor
    , infoLineColor :: ViewColor
    }
    deriving Show

defaultColorDef :: ColorDef
defaultColorDef = ColorDef
    { commentColor   = (100,155,100,255)
    , normalColor    = (128,128,128,255)
    , stringColor    = (100,100,155,255)
    , highlightColor = (200,200,100,255)
    , majColor       = (  0,  0,  0,255)
    , emptyColor     = (255,255,255,  0)
    , viewColor      = (200,200,255,255)
    , keywordColor   = (100,100,255,255)
    , typeColor      = (100,100,255,255)

    , errorLineColor   = (255,   0,   0, 128)
    , warningLineColor = (  0, 255, 255, 128)
    , infoLineColor    = (  0,   0, 255, 128)
    }

--------------------------------------------------
----            Color conf parsing
--------------------------------------------------
readHex :: Char -> Int
readHex c | 'a' <= c && c <= 'f' = fromEnum c - fromEnum 'a' + 10
          | 'A' <= c && c <= 'F' = fromEnum c - fromEnum 'A' + 10
          | '0' <= c && c <= '9' = read [c]
          | otherwise = 0

split :: Char -> String -> [String]
split _ "" =  []
-- Somehow GHC doesn't detect the selector thunks in the below code,
-- so s' keeps a reference to the first line via the pair and we have
-- a space leak (cf. #4334).
-- So we need to make GHC see the selector thunks with a trick.
split c s = cons (case break (== c) s of
        (l, s') -> (l, case s' of
                         []     -> []
                         _:s'' -> split c s''))
  where cons ~(h, t) = h : t

parseHtmlColor :: String -> Maybe ViewColor
parseHtmlColor ['#', r1, r2, g1, g2, b1, b2, a1, a2] = Just (r, g, b, a)
    where r = readHex r1 * 16 + readHex r2
          g = readHex g1 * 16 + readHex g2
          b = readHex b1 * 16 + readHex b2
          a = readHex a1 * 16 + readHex a2
parseHtmlColor ['#', r1, r2, g1, g2, b1, b2] = Just (r, g, b, 255)
    where r = readHex r1 * 16 + readHex r2
          g = readHex g1 * 16 + readHex g2
          b = readHex b1 * 16 + readHex b2
parseHtmlColor _ = Nothing

parseColorDef :: String -> ColorDef
parseColorDef txt = foldl' updateColorDef defaultColorDef vals
    where cleanSecondPart (a, []) = (a, [])
          cleanSecondPart (a, _:xs) = (a, xs)
          vals = map (cleanSecondPart . break ('=' ==)) 
               . concatMap (split ';')
               $ lines txt

updateColorDef :: ColorDef -> (String, String) -> ColorDef
updateColorDef def ("comment",val) =
    maybe def (\c -> def { commentColor = c }) $ parseHtmlColor val
updateColorDef def ("normal",val) =
    maybe def (\c -> def { normalColor = c }) $ parseHtmlColor val
updateColorDef def ("string",val) =
    maybe def (\c -> def { stringColor = c }) $ parseHtmlColor val
updateColorDef def ("highlight",val) =
    maybe def (\c -> def { highlightColor = c }) $ parseHtmlColor val
updateColorDef def ("maj",val) =
    maybe def (\c -> def { majColor = c }) $ parseHtmlColor val
updateColorDef def ("empty",val) =
    maybe def (\c -> def { emptyColor = c }) $ parseHtmlColor val
updateColorDef def ("view",val) =
    maybe def (\c -> def { viewColor = c }) $ parseHtmlColor val
updateColorDef def ("keyword",val) =
    maybe def (\c -> def { keywordColor = c }) $ parseHtmlColor val
updateColorDef def ("type",val) =
    maybe def (\c -> def { typeColor = c }) $ parseHtmlColor val

updateColorDef def ("errorLine",val) =
    maybe def (\c -> def { errorLineColor = c }) $ parseHtmlColor val
updateColorDef def ("warningLine",val) =
    maybe def (\c -> def { warningLineColor = c }) $ parseHtmlColor val
updateColorDef def ("infoLine",val) =
    maybe def (\c -> def { infoLineColor = c }) $ parseHtmlColor val

updateColorDef def _ = def

--------------------------------------------------
----            Language definitions
--------------------------------------------------
cCodeDef, haskellCodeDef, ocamlCodeDef,
             rubyCodeDef, shellLikeCodeDef,
             htmlCodeDef, emptyCodeDef,
             pythonCodeDef :: CodeDef

emptyCodeDef = CodeDef
            { lineComm = Nothing
            , multiLineCommBeg = Nothing
            , multiLineCommEnd = Nothing
            , tabSpace = 4
            , identParser = basicIdent
            , strParser = Nothing
            , keywordList = Set.empty
            , typeList = Set.empty
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

pythonCodeDef = shellLikeCodeDef
           { identParser = identWithPrime
           , strParser = Just $ stringParser False pythonCodeDef
           , keywordList = Set.fromList
                [ "def", "if", "while", "for", "in", "class"
                , "import", "from", "return", "break", "continue"
                , "not", "try", "with", "finally" ]

           , typeList = Set.fromList
                [ "int", "float", "string" ]
           }

cCodeDef = CodeDef
           { lineComm = Just "//"
           , multiLineCommBeg = Just "/*"
           , multiLineCommEnd = Just "*/"
           , tabSpace = 4
           , identParser = identWithPrime
           , strParser = Just $ stringParser False cCodeDef
           , keywordList = Set.fromList
                [ "do", "while", "for", "if", "else", "typedef"
                , "struct", "class", "public", "private"
                , "protected", "switch", "case", "const" ]
           , typeList = Set.fromList
                [ "void", "int", "short", "unsigned", "char"
                , "float", "double", "byte" ]
           }

haskellCodeDef = CodeDef
                 { lineComm = Just "--"
                 , multiLineCommBeg = Just "{-"
                 , multiLineCommEnd = Just "-}"
                 , tabSpace = 4
                 , identParser = identWithPrime
                 , strParser = Just $ stringParser False haskellCodeDef
                 , keywordList = Set.fromList
                    [ "let", "in", "where", "class", "instance"
                    , "data", "type", "newtype", "module", "import"
                    , "infixl", "infixr", "if", "then", "else", "qualified"
                    ]
                 , typeList = Set.fromList
                    [ "Bool", "Int", "Integer", "Float"
                    , "Double", "Set", "Map", "Char", "String" ]
                 }

ocamlCodeDef = CodeDef
               { lineComm = Nothing
               , multiLineCommBeg = Just "(*"
               , multiLineCommEnd = Just "*)"
               , tabSpace = 4
               , identParser = basicIdent
               , strParser = Just $ stringParser False ocamlCodeDef
               , keywordList = Set.fromList
                    [ "let", "in", "and", "match", "if", "then"
                    , "else", "module", "sig", "begin", "end"
                    , "class"
                    ]
               , typeList = Set.fromList
                    [ "bool", "int", "char", "float" ]
               }

--------------------------------------------------
----            Generation code
--------------------------------------------------
-- | Basic identifier parser parser the [a-zA-Z][a-zA-Z0-9']*
-- identifier
identWithPrime :: Char -> Int -> Bool
identWithPrime c 0 = isAlpha c
identWithPrime c _ = isAlphaNum c || c == '\''


-- | Basic identifier parser parser the [a-zA-Z][a-zA-Z0-9]*
-- identifier
basicIdent :: Char -> Int -> Bool
basicIdent c 0 = isAlpha c
basicIdent c _ = isAlphaNum c


-- | Parse a string, ignoring the \\\"
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
                  colorKey = keywordColor colorDef
                  colorType = typeColor colorDef

                  keyList = keywordList codeDef
                  typesList = typeList codeDef

                  prepareWord w | w `elem` highlightList = replicate (length w) colorHi
                                | w `Set.member` keyList = replicate (length w) colorKey
                                | w `Set.member` typesList = replicate (length w) colorType
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
addOverLines colordef errorList = snd . mapAccumL lineMarker sortedErrors . zip [1..]
  where sortedErrors = sortBy (\(_,line) (_,line') -> compare line line') errorList
        lineMarker [] (_, e) = ([], e)
        lineMarker fullList@((hiKind, lineNumber):xs) element@(currLine, e)
          | lineNumber < currLine = lineMarker xs element
          | lineNumber > currLine = (fullList, e)
          -- lineNumber == currLine
          | otherwise = (xs, highlightLine hiKind e)

        errorColor = errorLineColor colordef
        warningColor = warningLineColor colordef
        infoColor = infoLineColor colordef

        highlightLine ('i':_) line = map (\a -> alphaBlend a infoColor) line
        highlightLine ('I':_) line = map (\a -> alphaBlend a infoColor) line
        highlightLine ('w':_) line = map (\a -> alphaBlend a warningColor) line
        highlightLine ('W':_) line = map (\a -> alphaBlend a warningColor) line
        highlightLine _ line = map (\a -> alphaBlend a errorColor) line

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

