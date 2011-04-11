module CodeOverviewGenerator.Language.Python ( pythonCodeDef ) where

import qualified Data.Map as Map

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Language.Shell
import CodeOverviewGenerator.Color

           {-, keywordList = Set.fromList-}
                {-[ "def", "if", "while", "for", "in", "class"-}
                {-, "import", "from", "return", "break", "continue"-}
                {-, "not", "try", "with", "finally" ]-}

           {-, typeList = Set.fromList-}
                {-[ "int", "float", "string" ]-}

pythonCodeDef :: ColorDef -> CodeDef
pythonCodeDef colors = def
    where def = (shellCodeDef colors)
           { identParser = identWithPrime
           , strParser = Just $ stringParser False def
           , specialIdentifier = Map.empty
           }

