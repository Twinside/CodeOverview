module CodeOverviewGenerator.Language ( CodeDef( .. )
                                      , NextParse( .. )
                                      , ParseResult
                                      , Parser
                                      , emptyCodeDef
                                      , basicIdent
                                      ) where

import Data.Char
import qualified Data.Set as Set
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
      -- | Keyword list, for better coloration.
    , keywordList :: Set.Set String
      -- | Type list, for better coloration.
    , typeList :: Set.Set String
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
            , keywordList = Set.empty
            , typeList = Set.empty
            }

