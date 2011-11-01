-- | Shell language code def, only highlight comments.
module CodeOverviewGenerator.Language.Shell ( shellCodeDef ) where

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

-- | Shell language definition.
shellCodeDef :: CodeDef [CodeEntity]
shellCodeDef = def
    where def = emptyCodeDef
            { lineComm = strComment  "#"
            , tabSpace = 4
            , strParser = Just $ stringParser False def
            }

