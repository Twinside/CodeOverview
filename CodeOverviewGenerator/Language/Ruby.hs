module CodeOverviewGenerator.Language.Ruby ( rubyCodeDef ) where

{-import qualified Data.Map as Map-}

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Language.Shell
import CodeOverviewGenerator.Color

rubyCodeDef :: CodeDef [CodeEntity]
rubyCodeDef = def
    where def = shellCodeDef
            { multiLineCommBeg = strComment "=begin"
            , multiLineCommEnd = strComment "=end"
            }

