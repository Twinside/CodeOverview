-- | Module in charge of loading the diff overlay information
module CodeOverviewGenerator.DiffOverlay( loadOverlayFile
                                        , parseOverlayFile
                                        ) where

import Control.Applicative( (<$>) )
import Control.Arrow( (***) )
import Data.Maybe( catMaybes )
import CodeOverviewGenerator.CodeOverview( CodeOverlays )
import CodeOverviewGenerator.Language

loadOverlayFile :: FilePath -> IO CodeOverlays
loadOverlayFile path = parseOverlayFile <$> readFile path

-- | Parse lines of a fil defining a diff of the following form :
--  "+:35:6"
--  /- -- -----> size
--  |    \_____ begin line
--  |
-- diff action
-- actions are 
--
--  * '+' : diff add
--  * '-' : diff deletion
--  * '~' : diff deletion then add
--  * '+' : diff add then deletion
--
parseOverlayFile :: String -> CodeOverlays
parseOverlayFile txt = catMaybes . map parseLine $ lines txt
  where parseLine ('+':':':rest) = parseOffsets DiffAddEntity        rest
        parseLine ('-':':':rest) = parseOffsets DiffDelEntity        rest
        parseLine ('~':':':rest) = parseOffsets DiffDelThenAddEntity rest
        parseLine ('!':':':rest) = parseOffsets DiffAddThenDelEntity rest
        parseLine _ = Nothing

        parseOffsets kind str = Just $ (kind, offset, size)
          where (offset, size) = (read *** read . tail) $ break (== ':') str

