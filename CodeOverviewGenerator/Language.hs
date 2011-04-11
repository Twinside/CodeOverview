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
      -- | Coloration for keywords/identifier.
    , specialIdentifier :: Map.Map String ViewColor
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

