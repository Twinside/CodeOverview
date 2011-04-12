{-# LANGUAGE ViewPatterns #-}
module CodeOverviewGenerator.Language ( CodeDef( .. )
                                      , NextParse( .. )
                                      , ParseResult
                                      , Parser
                                      , emptyCodeDef
                                      , basicIdent
                                      , identWithPrime 
                                      , stringParser 
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
            }

-- | Basic identifier parser parser the [a-zA-Z][a-zA-Z0-9']*
-- identifier
identWithPrime :: Char -> Int -> Bool
identWithPrime c 0 = isAlpha c
identWithPrime c _ = isAlphaNum c || c == '\''

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

