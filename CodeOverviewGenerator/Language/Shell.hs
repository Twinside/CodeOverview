module CodeOverviewGenerator.Language.Shell ( shellCodeDef ) where

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

shellCodeDef :: ColorDef -> CodeDef [ViewColor]
shellCodeDef _colors = def
    where def = emptyCodeDef
            { lineComm = strComment  "#"
            , tabSpace = 4
            , strParser = Just $ stringParser False def
            }

