module CodeOverviewGenerator.Language.Java ( javaCodeDef ) where

import CodeOverviewGenerator.Language.C
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

javaStatement, javaAccess, javaType, javaExceptions, 
    javaStructure, javaTypedef, javaRepeat, javaBranch,
    javaBoolean, javaConditional  :: [String]
javaStatement = ["return","assert"]
javaAccess = ["public", "protected", "private"]
javaType = ["boolean", "char", "byte", "short", "int", "long"
           ,"float","double","void"]
javaTypedef = ["this", "super"]
javaExceptions = ["throw", "try", "catch"]
javaRepeat =  ["while", "for", "do"]
javaConditional = ["if", "then", "else", "switch"]
javaBranch = ["break", "continue"]
javaBoolean = ["true", "false"]
javaStructure = ["class", "enum", "extends", "implements", "interface"]

-- storageClass
{-static synchronized transient volatile final strictfp serializable-}
javaCodeDef :: ColorDef -> CodeDef [ViewColor]
javaCodeDef colors = def
    where cdef = (cCodeDef colors)
          def = cdef {
            specialIdentifier = prepareKeywords colors
                [(javaStatement, statementColor)
                ,(javaAccess, statementColor)
                ,(javaType, typeColor)
                ,(javaExceptions, exceptionColor)
                ,(javaStructure, structureColor)
                ,(javaTypedef, typedefColor)
                ,(javaRepeat, repeatColor)
                ,(javaConditional, conditionalColor)
                ,(javaBranch, conditionalColor)
                ,(javaBoolean, boolColor)
                ]

            }

