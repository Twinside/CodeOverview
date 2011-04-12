module CodeOverviewGenerator.Language.C( cCodeDef, cppCodeDef ) where

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
import qualified Data.Map as Map

cStatement, cLabel, cConditional, cRepeat, cType, cStructure,
    cStorageClass :: [String]
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
cStructure = ["struct", "union", "enum", "typedef"]
cStorageClass = ["static", "register", "auto", "volatile", "extern", "const", "inline"]

cCodeDef :: ColorDef -> CodeDef
cCodeDef colors = def
    where def = CodeDef
           { lineComm = strComment "//"
           , multiLineCommBeg = strComment "/*"
           , multiLineCommEnd = strComment "*/"
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
           }

cppStatement, cppAccess, cppType, cppExceptions, 
    cppOperator, cppStructure :: [String]
cppStatement = ["new", "delete", "this", "friend", "using"]
cppAccess = ["public", "protected", "private"]
cppType = ["inline", "virtual", "explicit", "export", "bool", "wchar_t"]
cppExceptions = ["throw", "try", "catch"]
cppOperator = ["operator", "typeid", "and", "bitor", "or", "xor", "compl"
              ,"bitand", "and_eq", "or_eq", "xor_eq", "not", "not_eq"
              , "class", "typename", "template", "namespace" ]
{-cppBoolean = ["true", "false"]-}
cppStructure = ["class", "typename", "template", "namespace"]

cppCodeDef :: ColorDef -> CodeDef
cppCodeDef colors = def
    where cdef = (cCodeDef colors)

          cppIdents = prepareKeywords colors
            [(cppStatement, statementColor)
            ,(cppAccess, statementColor)
            ,(cppType, typeColor)
            ,(cppExceptions, exceptionColor)
            ,(cppStructure, structureColor)
            ,(cppOperator, operatorColor)
            ]

          def = cdef {
            specialIdentifier = cppIdents `Map.union` specialIdentifier cdef 
            }
