module CodeOverviewGenerator.Language.Ruby ( rubyCodeDef ) where

{-import qualified Data.Map as Map-}

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Language.Shell
import CodeOverviewGenerator.Color

rubyCodeDef :: ColorDef -> CodeDef
rubyCodeDef colors = def
    where shell = shellCodeDef colors
          def = shell
            { multiLineCommBeg = strComment "=begin"
            , multiLineCommEnd = strComment "=end"
            }

