module CodeOverviewGenerator.Language.Haskell ( haskellCodeDef ) where

import qualified Data.Map as Map

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

                {-, keywordList = Set.fromList-}
                   {-[ "let", "in", "where", "class", "instance"-}
                   {-, "data", "type", "newtype", "module", "import"-}
                   {-, "infixl", "infixr", "if", "then", "else", "qualified"-}
                   {-]-}
                {-, typeList = Set.fromList-}
                   {-[ "Bool", "Int", "Integer", "Float"-}
                   {-, "Double", "Set", "Map", "Char", "String" ]-}

haskellCodeDef :: ColorDef -> CodeDef
haskellCodeDef _colors = def
    where def = CodeDef
                { lineComm = Just "--"
                , multiLineCommBeg = Just "{-"
                , multiLineCommEnd = Just "-}"
                , tabSpace = 4
                , identParser = identWithPrime
                , strParser = Just $ stringParser False def
                , specialIdentifier = Map.empty
                }

