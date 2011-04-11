module CodeOverviewGenerator.Language.C( cCodeDef ) where

import qualified Data.Map as Map

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

cStatement, cLabel, cConditional, cRepeat, cType :: [String]
cStatement = ["goto", "break", "return", "continue", "asm"]
cConditional = ["if", "else", "switch"]
cLabel = ["case", "default"]
cRepeat = ["while", "for", "do"]
cType = [ "int", "long", "short", "char", "void", "signed", "unsigned"
        , "float", "double", "size_t", "ssize_t", "off_t", "wchar_t"
        , "ptrdiff_t", "sig_atomic_t", "fpos_t", "clock_t", "time_t"
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

cCodeDef :: ColorDef -> CodeDef
cCodeDef colors = def
    where def = CodeDef
           { lineComm = Just "//"
           , multiLineCommBeg = Just "/*"
           , multiLineCommEnd = Just "*/"
           , tabSpace = 4
           , identParser = identWithPrime
           , strParser = Just $ stringParser False def
           , specialIdentifier = Map.fromList $
                prepareKeywords cStatement (statementColor colors)
             ++ prepareKeywords cLabel (labelColor colors)
             ++ prepareKeywords cRepeat (repeatColor colors)
             ++ prepareKeywords cType (typeColor colors)
             ++ prepareKeywords cConditional  (conditionalColor colors)
           }

