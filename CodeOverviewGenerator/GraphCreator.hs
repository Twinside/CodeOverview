module CodeOverviewGenerator.GraphCreator( createIncludeGraph ) where

import Control.Monad.State
import qualified Data.Map as M
import System.FilePath
import System.IO
import System.Directory

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.CodeOverview
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

type FileCollection = M.Map FilePath Int

isFileDrawed :: FilePath -> StateT FileCollection IO (Maybe Int)
isFileDrawed path = do
    visited <- get
    return $ path `M.lookup` visited

addPath :: FilePath -> StateT FileCollection IO Int
addPath path = do
    visited <- get
    let newId = M.size visited + 1
    put $ M.insert path newId visited
    return newId

-- | Create a graph visualisation of a bunch of files.
-- Follow includes files and include them in the graph.
-- The graph is created in graphviz format to be used
-- with the tools \'dot\' or \'neato\'.
createIncludeGraph :: (FilePath -> ColorDef -> CodeDef [ViewColor]) -- ^ Map a file to a parser.
                   -> ColorDef   -- ^ Color definition used to create code thumbnail.
                   -> FilePath   -- ^ Path for the graph writing.
                   -> [FilePath] -- ^ Directory used to track includes
                   -> [FilePath] -- ^ Initial file list to draw.
                   -> IO ()
createIncludeGraph codeMapper colorDef 
                   graphVizFile includeDirectory initialFiles = 
  flip evalStateT M.empty $ do
   currDir <- liftIO getCurrentDirectory
   outHandle <- liftIO $ openFile graphVizFile WriteMode
   liftIO $ hPutStrLn outHandle "digraph g {"
   mapM_ (getFileSubId outHandle . (currDir </>)) initialFiles
   liftIO $ hPutStrLn outHandle "}"
   liftIO $ hClose outHandle
  where getFileSubId :: Handle -> FilePath -> StateT FileCollection IO Int
        getFileSubId handle path = do
            drawed <- isFileDrawed path
            case drawed of
                Nothing -> fileProcess handle path
                Just i -> return i

        fileProcess :: Handle -> FilePath -> StateT FileCollection IO Int
        fileProcess handle filePath = do
            fileId <- addPath filePath
            file <- liftIO $ B.readFile filePath
            let parser = codeMapper filePath colorDef
                (img, context) = createCodeOverview parser
                                        colorDef [] []
                                        (B.lines file)
                foundFiles = linkedDocuments context
                outFileName = "file" ++ show fileId ++ ".png"
            when (not $ null img)
                 (liftIO $ savePng24BitAlpha outFileName img)

            liftIO . hPutStrLn handle $ 
                "p" ++ show fileId ++ " [image=\"" 
                                ++ outFileName ++  "\"]"

            forM_ foundFiles (\f -> do
                expandFilename <- liftIO $ expandPath includeDirectory    
                                                    filePath f
                case expandFilename of
                    Nothing -> return ()
                    Just fullPath -> do
                        idx <- getFileSubId handle fullPath
                        liftIO . hPutStrLn handle $ 
                            "p" ++ show fileId ++ " -> p" ++ show idx ++ ";"
                )
            
            return fileId


