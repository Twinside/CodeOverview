{-# LANGUAGE ViewPatterns #-}
module CodeOverviewGenerator.Language.C( cCodeDef ) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Char
import Data.Maybe( fromJust )
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

at :: (Int, B.ByteString) -> (Int, Maybe (Char, B.ByteString))
at (n, buff) = if B.length buff <= n
    then (n, Nothing)
    else (n, Just (B.index buff n, buff))

-- | '#..... ' -> (beforeCount, ".....", spacecount)
preprocParser :: Parser (Int, B.ByteString, Int)
preprocParser = Parser $ innerParser
  where innerParser (uncons -> Just ('#', toParse)) =
            let (preprocCommandStart, _) = eatWhiteSpace 4 toParse
            in return $ preprocParse (1 + preprocCommandStart, toParse)
        innerParser _ = return NoParse

        preprocParse (at -> (n, Nothing)) = Result ((n, B.empty, 0), B.empty)
        preprocParse (at -> (n,  Just (c,rest))) 
          | isAlpha c = preprocParse (n + 1, rest)
          | otherwise = Result ((n, B.take n rest, sp + 1), wholeRest)
              where (sp, wholeRest) = eatWhiteSpace 4 rest
        preprocParse _ = error "Compiler pleaser preprocParser"

parseInclude :: B.ByteString -> Maybe LinkedFile
parseInclude str = case between '"' '"' str of
    Just s -> Just . LocalInclude $ B.unpack s
    Nothing -> maybe Nothing (Just . SystemInclude . B.unpack) 
            $ between '<' '>' str

includeParser :: ColorDef -> ((Int, B.ByteString, Int),B.ByteString) 
              -> State ColoringContext (ParseResult [ViewColor])
includeParser colors ((initSize, command, n), rest) = do
  when (includeFile /= Nothing)
       (addIncludeFile $ fromJust includeFile)
  return $ Result (colorLine, B.empty)
    where incColor = stringColor colors
          spaceColor = emptyColor colors
          preproColor = preprocColor colors

          includeFile = parseInclude $ B.drop (initSize + n) rest

          colorLine = replicate (initSize + B.length command + 1) preproColor
                    ++ replicate n spaceColor
                    ++ replicate (B.length rest) incColor


preprocAdvancedList :: M.Map B.ByteString
                            (ColorDef -> ((Int, B.ByteString, Int),B.ByteString) 
                                      -> State ColoringContext (ParseResult [ViewColor]))
preprocAdvancedList = M.fromList
    [ (B.pack "include", includeParser)
    {-, (B.pack "if", semiCommentParser)-}
    ]

displayPreproc :: ColorDef -> (Int, B.ByteString, Int) -> [ViewColor]
displayPreproc colorDef (n, command, spaceCount) = colors
    where totalSize = n + spaceCount + B.length command
          colors = replicate totalSize $ preprocColor colorDef

preprocHighlighter :: ColorDef -> Parser [ViewColor]
preprocHighlighter colorDef = Parser $ \bt -> 
    let (Parser realParser) = preprocParser
    in realParser bt >>= resAnalyzer
  where resAnalyzer (NextParse (payload, _)) =
          return $ Result (displayPreproc colorDef payload, B.empty)
        resAnalyzer NoParse = return NoParse
        resAnalyzer (Result payload@(parsedCommand@(_, command, _) , _rest)) =
          case command `M.lookup` preprocAdvancedList of
              Just parser -> parser colorDef payload
              Nothing -> return $ Result
                  (displayPreproc colorDef parsedCommand, B.empty)

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

