module CodeOverviewGenerator.Language.Html ( htmlCodeDef ) where

{-import qualified Data.Map as Map-}

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

htmlCodeDef :: ColorDef -> CodeDef
htmlCodeDef _colors = emptyCodeDef
            { multiLineCommBeg = Just "<!--"
            , multiLineCommEnd = Just "-->"
            }

