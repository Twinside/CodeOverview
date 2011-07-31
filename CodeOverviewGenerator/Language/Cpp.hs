module CodeOverviewGenerator.Language.Cpp (cppCodeDef) where

import CodeOverviewGenerator.Language.C
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
import qualified Data.Map as Map

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

cppCodeDef :: CodeDef [CodeEntity]
cppCodeDef = def
    where cppIdents = prepareKeywords
            [(cppStatement, StatementEntity)
            ,(cppAccess, StatementEntity)
            ,(cppType, TypeEntity)
            ,(cppExceptions, ExceptionEntity)
            ,(cppStructure, StructureEntity)
            ,(cppOperator, OperatorEntity)
            ]

          def = cCodeDef {
            specialIdentifier = cppIdents `Map.union` specialIdentifier cCodeDef
            }

