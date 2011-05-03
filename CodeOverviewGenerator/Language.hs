{-# LANGUAGE ViewPatterns #-}
module CodeOverviewGenerator.Language ( -- * Types
                                        CodeDef( .. )
                                      , ColoringContext(..)
                                      , NextParse( .. )
                                      , LinkedFile( .. )
                                      , ParseResult
                                      , Parser

                                        -- * Default configurations
                                      , defaultColoringContext 
                                      , emptyCodeDef

                                        -- * Parsers
                                      , addIncludeFile
                                      , basicIdent
                                      , between
                                      , identWithPrime 
                                      , stringParser 
                                      , charParser 
                                      , intParser 
                                      , eatWhiteSpace
                                      ) where

import Control.Monad.State
import Data.Char
import qualified Data.Map as Map
import CodeOverviewGenerator.Color

import CodeOverviewGenerator.ByteString(uncons)
import qualified CodeOverviewGenerator.ByteString as B

newtype NextParse a =
    NextParse (a, Parser a)

type ParseResult a =
    Either (NextParse a)
           (Maybe (a, B.ByteString))

data LinkedFile =
      LocalInclude String
    | SystemInclude String
    deriving (Eq, Show)

data ColoringContext = ColoringContext
    { linkedDocuments :: [LinkedFile]
    , parsingDepth :: Int
    }
    deriving Show

defaultColoringContext :: ColoringContext
defaultColoringContext = ColoringContext
    { linkedDocuments = []
    , parsingDepth = 0
    }

type Parser a = B.ByteString -> State ColoringContext (ParseResult a)

-- | Define a language used by the image generator to put
-- some colors in it.
data    CodeDef a = CodeDef
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
    , strParser :: Maybe (ColorDef -> Parser a)
      -- | How we must transform tab into space.
    , tabSpace :: Int
      -- | Coloration for keywords/identifier.
    , specialIdentifier :: Map.Map B.ByteString ViewColor
      -- | List of special parsers used for a specific language.
    , specificParser :: [Parser a]
    }

-- | Basic identifier parser parser the [a-zA-Z][a-zA-Z0-9]*
-- identifier
basicIdent :: Char -> Int -> Bool
basicIdent c 0 = isAlpha c
basicIdent c _ = isAlphaNum c

addIncludeFile :: LinkedFile -> State ColoringContext ()
addIncludeFile f = do
    ctxt <- get
    put $ ctxt { linkedDocuments = f : linkedDocuments ctxt }

-- | Empty code def, should work without anything else
emptyCodeDef :: CodeDef a
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
intParser :: ColorDef -> Parser [ViewColor]
intParser colorDef (uncons -> Just (c, toParse))
    | isDigit c = return $ intParse (1, toParse)
        where numColor = numberColor colorDef
              intParse (n, uncons -> Just (c', rest))
                | isDigit c' = intParse (n + 1, rest)
                | otherwise = Right $ Just (replicate n numColor, rest)
              intParse (n, uncons -> Nothing) =
                Right $ Just (replicate n numColor, B.empty)
              intParse _ = error "Compiler pleaser - intParser"
intParser _ _ = return $ Right Nothing

-- | Aim to parse \' \' like structures (char representation) of a given
-- programming language.
charParser :: ColorDef -> Parser [ViewColor]
charParser colorDef (uncons -> Just ('\'', rest)) = return $ parser (1,rest)
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
charParser _ _ = return $ Right Nothing

-- | Parse a string, ignoring the \\\"
stringParser :: Bool -> CodeDef [ViewColor] -> ColorDef -> Parser [ViewColor]
stringParser allowBreak codeDef colorDef (uncons -> Just ('"',stringSuite)) =
  stringer (color:) stringSuite
    where color = stringColor colorDef
          empty = emptyColor colorDef
          tabSize = tabSpace codeDef
          stringer acc (uncons -> Nothing) = if allowBreak
                                then return . Left $ NextParse (acc [], stringer id)
                                else return $ Right Nothing
          stringer acc (uncons -> Just ('\\', uncons -> Just ('"',xs))) =
              stringer (acc . ([color, color]++)) xs
          stringer acc (uncons -> Just (' ',xs)) = stringer (acc . (empty:)) xs
          stringer acc (uncons -> Just ('\t',xs)) = 
            stringer (acc . (replicate tabSize empty ++)) xs
          stringer acc (uncons -> Just ('"',xs)) = return . Right $ Just (acc [color], xs)
          stringer acc (uncons -> Just (_,xs)) = stringer (acc . (color:)) xs
          stringer _ _ = error "stringParser compiler pleaser"
stringParser _ _ _ _ = return $ Right Nothing

-- | Parse a[^b]b where a is the first argument and b the second one
between :: Char -> Char -> B.ByteString -> Maybe B.ByteString
between a b (uncons -> Just (c, rest))
  | c /= a = Nothing
  | otherwise = Just $ B.takeWhile (/= b) rest
between _ _ _ = Nothing

-- | Eat all white space returning it's size and what's left to be
-- parsed.
eatWhiteSpace :: Int             -- ^ Size of space in number of cell
             -> B.ByteString   -- ^ String to parse
             -> (Int, B.ByteString) -- ^ Number of space ate and rest of string
eatWhiteSpace tabSize = eater 0
    where eater n (uncons -> Just (' ',rest)) = eater (n + 1) rest
          eater n (uncons -> Just ('\t',rest)) = eater (n + tabSize) rest
          eater n leftOver = (n, leftOver)

{-regionParser :: B.ByteString -> B.ByteString-}
             {--> Parser [ViewColor]-}
{-regionParser begin end = parseRegion-}
    {-where parseRegion str-}
            {-| begin `B.isPrefixOf` str-}
    
