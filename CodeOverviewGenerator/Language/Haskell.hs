module CodeOverviewGenerator.Language.Haskell ( haskellCodeDef ) where

import qualified Data.Map as Map

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

haskellCodeDef :: ColorDef -> CodeDef
haskellCodeDef colors = def
    where def = CodeDef
                { lineComm = Just "--"
                , multiLineCommBeg = Just "{-"
                , multiLineCommEnd = Just "-}"
                , tabSpace = 4
                , identParser = identWithPrime
                , strParser = Just $ stringParser False def
                , specialIdentifier = Map.fromList $
                    prepareKeywords (hModule ++ hStructure) (structureColor colors)
                 ++ prepareKeywords hImport (includeColor colors)
                 ++ prepareKeywords hTypedef (typedefColor colors)
                 ++ prepareKeywords hStatement (statementColor colors)
                 ++ prepareKeywords hConditional (conditionalColor colors)
                 ++ prepareKeywords hTypes (typeColor colors)
                }

