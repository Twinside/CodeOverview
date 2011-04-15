module CodeOverviewGenerator.Language.Haskell ( haskellCodeDef ) where

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

hStructure, hImport, hTypedef, hStatement, hConditional,
    hTypes, hModule :: [String]
hModule = ["module"]
hStructure = ["class", "data", "deriving", "instance", "default", "where"]
hImport = ["import"]
hTypedef = ["type", "newtype"]
hStatement = ["do", "case", "of", "let", "in"]
hConditional = ["if", "then", "else"]
hTypes = ["Int", "Integer", "Char", "Bool", "Float"
         , "Double", "IO", "Void", "Addr", "Array"
         , "String", "Maybe", "Either", "Ratio", "Complex"
         , "Ordering", "IOError", "IOResult", "ExitCode"]

haskellCodeDef :: ColorDef -> CodeDef [ViewColor]
haskellCodeDef colors = def
    where def = CodeDef
                { lineComm = strComment "--"
                , multiLineCommBeg = strComment "{-"
                , multiLineCommEnd = strComment "-}"
                , tabSpace = 4
                , identParser = identWithPrime
                , strParser = Just $ stringParser False def
                , specialIdentifier = prepareKeywords colors
                    [ (hModule ++ hStructure, structureColor)
                    , (hImport, includeColor)
                    , (hTypedef, typedefColor)
                    , (hStatement, statementColor)
                    , (hConditional, conditionalColor)
                    , (hTypes, typeColor)
                    ]

                , specificParser = [ intParser colors ]
                }

