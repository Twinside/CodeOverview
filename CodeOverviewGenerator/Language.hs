{-# LANGUAGE ViewPatterns #-}
module CodeOverviewGenerator.Language ( CodeDef( .. )
                                      , NextParse( .. )
                                      , ParseResult
                                      , Parser
                                      , emptyCodeDef
                                      , basicIdent
                                      , identWithPrime 
                                      , stringParser 
                                      , charParser 
                                      , intParser 
                                      ) where

import Data.Char
import qualified Data.Map as Map
import CodeOverviewGenerator.Color

import CodeOverviewGenerator.ByteString(uncons)
import qualified CodeOverviewGenerator.ByteString as B

newtype NextParse = NextParse ([ViewColor], Parser)
type ParseResult = Either NextParse (Maybe ([ViewColor], B.ByteString))
type Parser = B.ByteString -> ParseResult

-- | Define a language used by the image generator to put
-- some colors in it.
data    CodeDef = CodeDef
    { -- | Beginning marker for mono line market
      lineComm         :: Maybe B.ByteString
      -- | Beginning marker for multilines comments, like
      -- \'/*\' in C or \'{-\' in Haskell
    , multiLineCommBeg :: Maybe B.ByteString
      -- | End  marker for multiline comments, like
      -- \'*/\' in C or \'-}\' in Haskell
    , multiLineCommEnd :: Maybe B.ByteString
      -- | Definition for identifier for the current language.
    , identParser :: Char -> Int -> Bool
      -- | Definition for strings in the current language.
    , strParser :: Maybe (ColorDef -> Parser)
      -- | How we must transform tab into space.
    , tabSpace :: Int
      -- | Coloration for keywords/identifier.
    , specialIdentifier :: Map.Map B.ByteString ViewColor
      -- | List of special parsers used for a specific language.
    , specificParser :: [Parser]
    }

-- | Basic identifier parser parser the [a-zA-Z][a-zA-Z0-9]*
-- identifier
basicIdent :: Char -> Int -> Bool
basicIdent c 0 = isAlpha c
basicIdent c _ = isAlphaNum c

-- | Empty code def, should work without anything else
emptyCodeDef :: CodeDef
emptyCodeDef = CodeDef
            { lineComm = Nothing
            , multiLineCommBeg = Nothing
            , multiLineCommEnd = Nothing
            , tabSpace = 4
            , identParser = basicIdent
            , strParser = Nothing
            , specialIdentifier = Map.empty
            , specificParser = []
            }

-- | Basic identifier parser parser the [a-zA-Z][a-zA-Z0-9']*
-- identifier
identWithPrime :: Char -> Int -> Bool
identWithPrime c 0 = isAlpha c
identWithPrime c _ = isAlphaNum c || c == '\''

-- | Parse an integer of the form \'[0-9]+\'
intParser :: ColorDef -> Parser
intParser colorDef (uncons -> Just (c, toParse))
    | isDigit c = intParse (1, toParse)
        where numColor = numberColor colorDef
              intParse (n, uncons -> Just (c', rest))
                | isDigit c' = intParse (n + 1, rest)
                | otherwise = Right $ Just (replicate n numColor, rest)
              intParse (n, uncons -> Nothing) =
                Right $ Just (replicate n numColor, B.empty)
              intParse _ = error "Compiler pleaser - intParser"
intParser _ _ = Right Nothing

-- | Aim to parse \' \' like structures (char representation) of a given
-- programming language.
charParser :: ColorDef -> Parser
charParser colorDef (uncons -> Just ('\'', rest)) = parser (1,rest)
    where color = charColor colorDef
          parser (n, uncons -> Just ('\\', uncons -> Just ('\\',xs))) =
              parser (n + 2,xs)
          parser (n, uncons -> Just ('\\', uncons -> Just ('\'',xs))) =
              parser (n + 2,xs)
          parser (n, uncons -> Just ('\'', rest')) = 
                Right (Just (replicate (n + 1) color, rest'))
          parser (n, uncons -> Just (_, xs)) = parser (n + 1, xs)
          parser (n, uncons -> Nothing) =
                Right $ Just (replicate n color, B.empty)
          parser (_, _) = error "Compiler pleaser charParser"
charParser _ _ = Right Nothing

-- | Parse a string, ignoring the \\\"
stringParser :: Bool -> CodeDef -> ColorDef -> Parser
stringParser allowBreak codeDef colorDef (uncons -> Just ('"',stringSuite)) =
  stringer (color:) stringSuite
    where color = stringColor colorDef
          empty = emptyColor colorDef
          tabSize = tabSpace codeDef
          stringer acc (uncons -> Nothing) = if allowBreak
                                then Left $ NextParse (acc [], stringer id)
                                else Right Nothing
          stringer acc (uncons -> Just ('\\', uncons -> Just ('"',xs))) =
              stringer (acc . ([color, color]++)) xs
          stringer acc (uncons -> Just (' ',xs)) = stringer (acc . (empty:)) xs
          stringer acc (uncons -> Just ('\t',xs)) = 
            stringer (acc . (replicate tabSize empty ++)) xs
          stringer acc (uncons -> Just ('"',xs)) = Right $ Just (acc [color], xs)
          stringer acc (uncons -> Just (_,xs)) = stringer (acc . (color:)) xs
          stringer _ _ = error "stringParser compiler pleaser"
stringParser _ _ _ _ = Right Nothing

