module CodeOverviewGenerator.GraphCreator( createIncludeGraph ) where

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
{-import qualified CodeOverviewGenerator.ByteString as B-}

-- | Try to find a file within some directory list, if
-- not found, return Nothing
expandPath :: [FilePath] -> FilePath -> IO (Maybe FilePath)
expandPath _ _ = return Nothing

createIncludeGraph :: (FilePath -> CodeDef [ViewColor])
                   -> ColorDef -> FilePath -> [FilePath] -> [FilePath]
                   -> IO ()
createIncludeGraph codeMapper colorDef 
                   graphVizFile includeDirectory initialFiles =
    return ()

