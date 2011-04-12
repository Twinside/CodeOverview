module CodeOverviewGenerator.Language.Shell ( shellCodeDef ) where

{-import qualified Data.Map as Map-}

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

shellCodeDef :: ColorDef -> CodeDef
shellCodeDef _colors = def
    where def = emptyCodeDef
            { lineComm = strComment  "#"
            , tabSpace = 4
            , strParser = Just $ stringParser False def
            }

