{-# LANGUAGE ViewPatterns #-}
module CodeOverviewGenerator.Language.C( cCodeDef ) where

import Control.Applicative
import qualified Data.Map as M
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
import qualified CodeOverviewGenerator.ByteString as B

cStatement, cLabel, cConditional, cRepeat, cType, cStructure,
    cStorageClass :: [String]
cStatement = ["goto", "break", "return", "continue", "asm"]

cConditional = ["if", "else", "switch"]

cLabel = ["case", "default"]

cRepeat = ["while", "for", "do"]

cType = [ "int", "long", "short", "char", "void", "signed", "unsigned"
        , "float", "double", "size_t", "ssize_t", "off_t", "wchar_t"
        , "ptrdiff_t", "sig_atomic_t", "fp2408.339os_t", "clock_t", "time_t"
        , "va_list", "jmp_buf", "FILE", "DIR", "div_t", "ldiv_t"
        , "mbstate_t", "wctrans_t", "wint_t", "wctype_t", "bool"
        , "complex", "int8_t", "int16_t", "int32_t", "int64_t"
        , "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int_least8_t"
        , "int_least16_t", "int_least32_t", "int_least64_t"
        , "uint_least8_t", "uint_least16_t", "uint_least32_t"
        , "uint_least64_t", "int_fast8_t", "int_fast16_t"
        , "int_fast32_t", "int_fast64_t", "uint_fast8_t"
        , "uint_fast16_t", "uint_fast32_t", "uint_fast64_t", "intptr_t"
        , "uintptr_t", "intmax_t", "uintmax_t", "__label__"
        , "__complex__", "__volatile__"]

cStructure = ["struct", "union", "enum", "typedef"]

cStorageClass = ["static", "register", "auto", "volatile", "extern", "const", "inline"]

-- | '#..... ' -> (beforeCount, ".....", spacecount)
preprocParser :: Parser (Int, B.ByteString, Int)
preprocParser = const (,,) <$> charParse '#'
                           <*> eatWhiteSpace 4 
                           <*> identParse 
                           <*> eatWhiteSpace 4

parseInclude :: Parser (Maybe LinkedFile)
parseInclude =
        ((Just . LocalInclude . B.unpack) <$> between '"' '"' (notChars "\"") )
    <|> ((Just . SystemInclude . B.unpack) <$> between '<' '>' (notChars ">"))

includeParser :: (Int, B.ByteString, Int) -> Parser [CodeEntity]
includeParser (initSize, command, n) = do
  parseInclude >>= wrap parseResultAnalyze
    where wrap f a = Parser $ \bitString -> do
              v <- f a
              return $ Result (v, bitString)

          parseResultAnalyze Nothing = return $ colorLine 0
          parseResultAnalyze (Just f) = do
              addIncludeFile f
              return . colorLine $ lengthOfLinkeFile f

          colorLine incSize =
              replicate (initSize + B.length command + 1) PreprocEntity
            ++ replicate n EmptyEntity
            ++ replicate incSize StringEntity


preprocAdvancedList :: M.Map B.ByteString
                            ((Int, B.ByteString, Int) -> Parser [CodeEntity])
preprocAdvancedList = M.fromList
    [ (B.pack "include", includeParser)
    -- import is more an objective C thingy, but hey, it works fine.
    , (B.pack "import", includeParser)
    {-, (B.pack "if", semiCommentParser)-}
    ]

displayPreproc :: (Int, B.ByteString, Int) -> [CodeEntity]
displayPreproc (n, command, spaceCount) = replicate totalSize PreprocEntity
    where totalSize = n + spaceCount + B.length command

preprocHighlighter :: Parser [CodeEntity]
preprocHighlighter = preprocParser >>= resAnalyzer
  where resAnalyzer payload@(_, command, _) =
          case command `M.lookup` preprocAdvancedList of
              Nothing -> return $ displayPreproc payload
              Just parser -> parser payload

cCodeDef :: CodeDef [CodeEntity]
cCodeDef = def
    where def = CodeDef
           { lineComm = strComment "//"
           , multiLineCommBeg = strComment "/*"
           , multiLineCommEnd = strComment "*/"
           , recursiveComment = False
           , tabSpace = 4
           , identParser = identWithPrime
           , strParser = Just $ stringParser False def
           , specialIdentifier = prepareKeywords
                [ (cLabel, LabelEntity)
                , (cStatement, StatementEntity)
                , (cRepeat, RepeatEntity)
                , (cType, TypeEntity)
                , (cConditional, ConditionalEntity)
                , (cStructure, StructureEntity)
                , (cStorageClass, StorageClassEntity)
                ]
           , specificParser = [intParser, preprocHighlighter]
           , heatTokens = 
                [ heatToken    1 '{'
                , heatToken (-1) '}'
                , heatToken    1 '('
                , heatToken (-1) ')'
                , heatToken    1 '['
                , heatToken (-1) ']'
                ]
           }
           where heatToken i c = return (1, i) <$> charParse c

