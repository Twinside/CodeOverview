{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CodeOverviewGenerator.Language ( -- * Types
                                        CodeDef( .. )
                                      , ColoringContext(..)
                                      , LinkedFile( .. )
                                      , ParseResult( .. )
                                      , Parser( .. )

                                        -- * Default configurations
                                      , defaultColoringContext 
                                      , emptyCodeDef
                                      , lengthOfLinkeFile 

                                        -- * Parsers
                                      , token
                                      , nullParser 
                                      , addIncludeFile
                                      , basicIdent
                                      , between
                                      , charParse
                                      , anyChar
                                      , notChars
                                      , identWithPrime 
                                      , identParse 
                                      , stringParser 
                                      , charParser 
                                      , intParser 
                                      , eatWhiteSpace
                                      ) where

import Control.Applicative
import Control.Monad.State
import Data.Char
import qualified Data.Map as Map
import CodeOverviewGenerator.Color

import CodeOverviewGenerator.ByteString(uncons)
import qualified CodeOverviewGenerator.ByteString as B

data ParseResult a =
      NextParse (a, Parser a)
    | Result    (a, B.ByteString)
    | NoParse

data LinkedFile =
      LocalInclude String
    | SystemInclude String
    deriving (Eq, Show)

data ColoringContext = ColoringContext
    { linkedDocuments :: [LinkedFile]
    , parsingDepth :: Int
    }
    deriving Show

-- Return the size in number of character of the linked
-- file.
lengthOfLinkeFile :: LinkedFile -> Int
lengthOfLinkeFile (LocalInclude s) = length s
lengthOfLinkeFile (SystemInclude s) = length s


defaultColoringContext :: ColoringContext
defaultColoringContext = ColoringContext
    { linkedDocuments = []
    , parsingDepth = 0
    }

newtype Parser a =
    Parser { runParse :: B.ByteString -> State ColoringContext (ParseResult a) }

instance Functor Parser where
    fmap f (Parser parser) = Parser $ \bitString -> do
      rez <- parser bitString
      case rez of
        NextParse (a, np) -> return $ NextParse (f a, fmap f np)
        Result    (a, b) -> return $ Result (f a, b)
        NoParse -> return NoParse

instance Applicative Parser where
    pure val = Parser $ \b -> return $ Result (val, b)

    (<*>) (Parser mfunc) (Parser mval) = Parser $ \b -> do
        func <- mfunc b
        case func of
          NoParse -> return NoParse
          Result (f, rest) -> do
            rez <- mval rest
            case rez of
              NoParse -> return NoParse
              Result (val, left) -> return $ Result (f val, left)
              NextParse (val, np) -> return $ NextParse (f val, f `fmap`np)

          -- No meaningful way to use NextParse as first case I guess
          NextParse _ -> return NoParse

instance Monad Parser where
    return = pure
    (>>=) (Parser parser) f = Parser $ \bitString -> do
        parseRez <- parser bitString 
        case parseRez of
           NoParse -> return NoParse
           Result (rez, rest) ->
             let (Parser p) = f rez in p rest
           NextParse (rez, _) ->
             let (Parser p) = f rez in p B.empty


instance Alternative Parser where
    empty = Parser $ \_ -> return NoParse
    (<|>) (Parser pa) (Parser pb) = Parser $ \b -> do
        rez <- pa b
        case rez of
          NoParse -> pb b
          Result a -> return $ Result a
          NextParse a -> return $ NextParse a

type Count = Int
type DepthIncrease = Int

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

      -- | If the multi lines comment can be nested.
    , recursiveComment :: Bool
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
      -- | Pair of token to generate a "generic" heat map
    , heatTokens :: [Parser (Count, DepthIncrease)]
    }

data CodeEntity =
      CommentEntity
    | StringEntity
    | NormalEntity
    | HighlightEntity
    | MajEntity
    | EmptyEntity
    | ViewEntity
    | KeywordEntity
    | TypeEntity

    | LabelEntity
    | ConditionalEntity
    | RepeatEntity
    | StructureEntity
    | StatementEntity
    | PreprocEntity
    | MacroEntity
    | TypedefEntity
    | ExceptionEntity
    | OperatorEntity
    | IncludeEntity
    | StorageClassEntity
    | CharEntity
    | NumberEntity
    | FloatEntity
    | BoolEntity
    | FunctionEntity
    | TagEntity
    | AttribTagEntity
    deriving (Eq, Enum)

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
            , recursiveComment = False
            , heatTokens = []
            }

-- | Basic identifier parser parser the [a-zA-Z][a-zA-Z0-9']*
-- identifier
identWithPrime :: Char -> Int -> Bool
identWithPrime c 0 = isAlpha c
identWithPrime c _ = isAlphaNum c || c == '\''

-- | Parse an integer of the form \'[0-9]+\'
intParser :: Parser [CodeEntity]
intParser colorDef = Parser $ \toParse ->
    if not (B.null toParse) && isDigit (B.head toParse)
      then return $ intParse (0, toParse)
      else return NoParse
        where intParse (n, uncons -> Just (c', rest))
                | isDigit c' = intParse (n + 1, rest)
                | otherwise = Result (replicate n NumberEntity, rest)
              intParse (n, uncons -> Nothing) =
                Result (replicate n NumberEntity, B.empty)
              intParse _ = error "Compiler pleaser - intParser"

-- | Aim to parse \' \' like structures (char representation) of a given
-- programming language.
charParser :: Parser [CodeEntity]
charParser = Parser $ innerParser
    where innerParser (uncons -> Just ('\'', rest)) = return $ parser (1,rest)
          innerParser _ = return NoParse

          parser (n, uncons -> Just ('\\', uncons -> Just ('\\',xs))) =
              parser (n + 2,xs)
          parser (n, uncons -> Just ('\\', uncons -> Just ('\'',xs))) =
              parser (n + 2,xs)
          parser (n, uncons -> Just ('\'', rest')) = 
                Result (replicate (n + 1) color, rest')
          parser (n, uncons -> Just (_, xs)) = parser (n + 1, xs)
          parser (n, uncons -> Nothing) =
                Result (replicate n CharEntity, B.empty)
          parser (_, _) = error "Compiler pleaser charParser"

-- | Parse a string, ignoring the \\\"
stringParser :: Bool -> CodeDef [CodeEntity] -> Parser [CodeEntity]
stringParser allowBreak codeDef = Parser innerParser
  
    where innerParser (uncons -> Just ('"',stringSuite)) =
                stringer (color:) stringSuite
          innerParser _ = return NoParse

          color = stringColor colorDef
          emptyC = emptyColor colorDef
          tabSize = tabSpace codeDef

          stringer acc (uncons -> Nothing) = if allowBreak
                                then return $ NextParse (acc [], Parser $ stringer id)
                                else return NoParse
          stringer acc (uncons -> Just ('\\', uncons -> Just ('"',xs))) =
              stringer (acc . ([color, color]++)) xs
          stringer acc (uncons -> Just (' ',xs)) = stringer (acc . (emptyC:)) xs
          stringer acc (uncons -> Just ('\t',xs)) = 
            stringer (acc . (replicate tabSize emptyC ++)) xs
          stringer acc (uncons -> Just ('"',xs)) = return $ Result (acc [color], xs)
          stringer acc (uncons -> Just (_,xs)) = stringer (acc . (color:)) xs
          stringer _ _ = error "stringParser compiler pleaser"

anyChar :: Parser Char
anyChar = Parser innerParse
 where innerParse (uncons -> Just (c,rest)) = 
            return $ Result (c, rest)
       innerParse _ = return NoParse

charParse :: Char -> Parser Char
charParse a = Parser innerParse
    where innerParse (uncons -> Just (c,rest))
              | c == a = return $ Result (a, rest)
              | otherwise = return NoParse
          innerParse _ = return NoParse

-- | Parse a[^b]b where a is the first argument and b the second one
between :: Char -> Char -> Parser a -> Parser a
between a b parser = (\_ prez _ -> prez) <$> charParse a <*> parser <*> charParse b

notChars :: [Char] -> Parser B.ByteString
notChars lst = Parser $ fullParser
    where fullParser bitString = innerParser bitString 0 bitString
          innerParser bitString n (uncons -> Just (c, rest))
                | not $ c `elem` lst = innerParser bitString (n+1) rest
          innerParser bitString n rest 
                | n > 0 = return $ Result (B.take n bitString, rest)
                | otherwise = return NoParse

-- | Eat all white space returning it's size and what's left to be
-- parsed. Parse \'[ \\t]*\'
eatWhiteSpace :: Int            -- ^ Size of space in number of cell
              -> Parser Int     -- ^ Number of space ate
eatWhiteSpace tabSize = Parser $ eater 0
    where eater n (uncons -> Just (' ',rest)) = eater (n + 1) rest
          eater n (uncons -> Just ('\r',rest)) = eater (n + 1) rest
          eater n (uncons -> Just ('\n',rest)) = eater (n + 1) rest
          eater n (uncons -> Just ('\t',rest)) = eater (n + tabSize) rest
          eater n leftOver = return $ Result (n, leftOver)

token :: String -> Parser Int
token str = Parser innerParser
    where packed = B.pack str
          innerParser b
            | not $ packed `B.isPrefixOf` b = return NoParse
            | otherwise = if isFollowedByWhitespace rest
                 then return $ Result (B.length packed, rest)
                 else return NoParse
                where rest = B.drop (B.length packed) b
                      isFollowedByWhitespace (uncons -> Just (c, _))
                        | c == ' ' || c == '\t' = True
                      isFollowedByWhitespace _ = False 

nullParser :: Parser Int
nullParser = Parser $ \b -> return $ Result (0, b)

identParse :: Parser B.ByteString
identParse = Parser subParser
  where subParser bitString = innerParser bitString 0 bitString

        innerParser _     0 (uncons -> Nothing) = return NoParse
        innerParser whole _ (uncons -> Nothing) = return $ Result (whole, B.empty)
        innerParser whole n (uncons -> Just (c,rest)) 
          | isAlpha c = innerParser whole (n + 1) rest
          | n > 0 =  return $ Result (B.take n whole, B.drop n whole)
          | otherwise = return NoParse
        innerParser _ _ _ = error "Compiler pleaser identParser"

