module CodeOverviewGenerator.Language.Ocaml ( ocamlCodeDef ) where

import qualified Data.Map as Map
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

               {-, keywordList = Set.fromList-}
                    {-[ "let", "in", "and", "match", "if", "then"-}
                    {-, "else", "module", "sig", "begin", "end"-}
                    {-, "class"-}
                    {-]-}
               {-, typeList = Set.fromList-}
                    {-[ "bool", "int", "char", "float" ]-}

ocamlCodeDef :: ColorDef -> CodeDef
ocamlCodeDef _colors = def
    where def = CodeDef
               { lineComm = Nothing
               , multiLineCommBeg = strComment "(*"
               , multiLineCommEnd = strComment "*)"
               , tabSpace = 4
               , identParser = basicIdent
               , strParser = Just $ stringParser False def
               , specialIdentifier = Map.empty
               }

