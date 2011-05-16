{-# LANGUAGE ViewPatterns #-}
module CodeOverviewGenerator.Language.C( cCodeDef ) where

import Control.Applicative
import qualified Data.Map as M
import Data.Char
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
import CodeOverviewGenerator.ByteString( uncons )
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

identParse :: Parser B.ByteString
identParse = Parser subParser
  where subParser bitString = innerParser bitString 0 bitString

        innerParser _     0 (uncons -> Nothing) = return NoParse
        innerParser whole _ (uncons -> Nothing) = return $ Result (whole, B.empty)
        innerParser whole n (uncons -> Just (c,rest)) 
          | isAlpha c = innerParser whole (n + 1) rest
          | otherwise = return $ Result (B.take n whole , rest)
        innerParser _ _ _ = error "Compiler pleaser preprocParser"

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

includeParser :: ColorDef -> (Int, B.ByteString, Int) -> Parser [ViewColor]
includeParser colors (initSize, command, n) = do
  parseInclude >>= wrap parseResultAnalyze
    where incColor = stringColor colors
          spaceColor = emptyColor colors
          preproColor = preprocColor colors

          wrap f a = Parser $ \bitString -> do
              v <- f a
              return $ Result (v, bitString)

          parseResultAnalyze Nothing = return $ colorLine 0
          parseResultAnalyze (Just f) = do
              addIncludeFile f
              return . colorLine $ lengthOfLinkeFile f

          colorLine incSize =
              replicate (initSize + B.length command + 1) preproColor
            ++ replicate n spaceColor
            ++ replicate incSize incColor


preprocAdvancedList :: M.Map B.ByteString
                            (ColorDef -> (Int, B.ByteString, Int) 
                                      -> Parser [ViewColor])
preprocAdvancedList = M.fromList
    [ (B.pack "include", includeParser)
    {-, (B.pack "if", semiCommentParser)-}
    ]

displayPreproc :: ColorDef -> (Int, B.ByteString, Int) -> [ViewColor]
displayPreproc colorDef (n, command, spaceCount) = colors
    where totalSize = n + spaceCount + B.length command
          colors = replicate totalSize $ preprocColor colorDef

preprocHighlighter :: ColorDef -> Parser [ViewColor]
preprocHighlighter colorDef = preprocParser >>= resAnalyzer
  where resAnalyzer payload@(_, command, _) =
          case command `M.lookup` preprocAdvancedList of
              Nothing -> return $ displayPreproc colorDef payload
              Just parser -> parser colorDef payload

cCodeDef :: ColorDef -> CodeDef [ViewColor]
cCodeDef colors = def
    where def = CodeDef
           { lineComm = strComment "//"
           , multiLineCommBeg = strComment "/*"
           , multiLineCommEnd = strComment "*/"
           , recursiveComment = False
           , tabSpace = 4
           , identParser = identWithPrime
           , strParser = Just $ stringParser False def
           , specialIdentifier = prepareKeywords colors
                [ (cLabel, labelColor)
                , (cStatement, statementColor)
                , (cRepeat, repeatColor)
                , (cType, typeColor)
                , (cConditional, conditionalColor)
                , (cStructure, structureColor)
                , (cStorageClass, storageClassColor)
                ]
           , specificParser = [intParser colors, preprocHighlighter  colors]
           }

