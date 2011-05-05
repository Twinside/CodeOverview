module CodeOverviewGenerator.GraphCreator( createIncludeGraph ) where

import Control.Monad( filterM )
import System.FilePath
import System.Directory

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
import qualified CodeOverviewGenerator.ByteString as B

import Png

-- | Try to find a file within some directory list, if
-- not found, return Nothing
expandPath :: [FilePath] -> FilePath -> LinkedFile
           -> IO (Maybe FilePath)
expandPath paths currentFile (LocalInclude s) =
    let (currPath, _) = splitFileName currentFile
    in expandPath (currPath : paths) currentFile $ SystemInclude s
expandPath paths _ (SystemInclude s) = do
    lst <- filterM doesFileExist [p </> s | p <- paths ]
    case lst of
      (f:_) -> return $ Just f
      _ -> return Nothing

-- | Create a graph visualisation of a bunch of files.
-- Follow includes files and include them in the graph.
-- The graph is created in graphviz format to be used
-- with the tools \'dot\' or \'neato\'.
createIncludeGraph :: (FilePath -> CodeDef [ViewColor]) -- ^ Map a file to a parser.
                   -> ColorDef   -- ^ Color definition used to create code thumbnail.
                   -> FilePath   -- ^ Path for the graph writing.
                   -> [FilePath] -- ^ Directory used to track includes
                   -> [FilePath] -- ^ Initial file list to draw.
                   -> IO ()
createIncludeGraph codeMapper colorDef 
                   graphVizFile includeDirectory initialFiles =
  where fileProcess handle filePath n = do
            file <- B.readFile
            let parser = codeMapper filePath
                (img, context) = createCodeOverview parser
                                        colorDef [] []
                                        (B.lines file)
                outFileName = show n ++ ".png"
            when (not $ null img) (savePng24Bit outFileName img)
            hPutStrLn handle $

