import Control.Monad( when, forM_ )
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.CodeOverview
import CodeOverviewGenerator.GraphCreator
import CodeOverviewGenerator.LanguageAssociation
import qualified CodeOverviewGenerator.ByteString as B
import Png

import CodeOverviewGenerator.DiffOverlay

data OverOption = OverOption
      { overOut :: String -> String
      , overTextOut :: String -> String
      , overVersion :: Bool
      , overConf :: String
      , overHelp :: Bool
      , overTransparent :: Bool
      , overVerbose :: Bool
      , overTop :: Int
      , overHiSize :: Int
      , overFiles :: [FilePath]
      , overHighlighted :: [String]
      , overErrFile :: Maybe FilePath
      , overIncludeDirs :: [FilePath]
      , overGraph :: Bool
      , overFileFormat :: Maybe String
      , overRecursiveDiscovery :: Bool
      , overHeatMap :: Bool
      , overAsciiVersion :: Maybe Int
      , overDiffOverlays :: Maybe String
      }

defaultOption :: OverOption
defaultOption = OverOption
    { overOut = pngIzeExtension
    , overTextOut = txtIzeExtension
    , overVersion = False
    , overConf = ""
    , overHelp = False
    , overVerbose = False
    , overTransparent = False
    , overHiSize = 10
    , overTop = -1
    , overFiles = []
    , overHighlighted = []
    , overErrFile = Nothing
    , overGraph = False
    , overIncludeDirs = []
    , overRecursiveDiscovery = False
    , overFileFormat = Nothing
    , overHeatMap = False
    , overAsciiVersion = Nothing
    , overDiffOverlays = Nothing
    }

pngIzeExtension :: FilePath -> FilePath
pngIzeExtension = (++ ".png") . fst . splitExtension

txtIzeExtension :: FilePath -> FilePath
txtIzeExtension = (++ ".txt") . fst . splitExtension

commonOption :: [OptDescr (OverOption -> OverOption)]
commonOption =
    [ 
      Option "a" ["alpha"]   (NoArg (\o -> o{overTransparent = True}))
                               "Produce a png with transparency"
    , Option "h" ["help"]    (NoArg (\o -> o{ overHelp = True}))
                               "Display inline help"
    , Option "o" ["output"]  (ReqArg (\f o -> o{ overOut = const f
                                              , overTextOut = const f }) "FILE")
                               "Output FILE, by default same name with extension replaced."
    , Option "c" ["conf"]    (ReqArg (\f o -> o{ overConf = f}) "FILECONF")
                               "Configuration file, to configure colors."
    , Option []    ["vs"]      (ReqArg (\f o -> o{overHiSize=read f}) "Size")
                               "Viewport size (in lines)."
    , Option []    ["hi"]      (ReqArg (\f o -> o{overHighlighted = f:overHighlighted o}) "Highlight")
                               "Highlight a word"
    , Option "f" ["format"]    (ReqArg (\f o -> o{ overFileFormat = Just f }) "FileFormat")
                               "Define a file type for parsing"
    , Option "t" ["top"]     (ReqArg (\f o -> o{overTop=read f}) "TOP")
                               "Viewport beginning line"
    , Option "v" ["verbose"] (NoArg (\o -> o{overVerbose = True}))
                               "Show progression and various information"
    , Option "V" ["version"] (NoArg (\o -> o{overVersion = True}))
                               "Show version number and various information"
    , Option []  ["heatmap"] (NoArg (\o -> o{overHeatMap = True})) 
                                "Create a heatmap display of the source file"
    , Option []    ["graph"]   (NoArg (\o -> o{overGraph = True}))
                               "Create a graph from a bunch of code sources."
    , Option "r"   ["recursive-discovery"] (NoArg (\o -> o{ overRecursiveDiscovery = True }))
                               "Try to find source file for graph drawing automatically."
    , Option "I" ["include-dir"] (ReqArg (\f o -> o { overIncludeDirs = overIncludeDirs o ++ [f]}) "Directory")
                                   "Add a directory to search for include files."
    , Option []    ["errfile"]  (ReqArg (\f o -> o {overErrFile = Just f}) "FILENAME") "Error lines"
    , Option []    ["text"]   (ReqArg (\f o -> o {overAsciiVersion = Just $ read f }) "BlockSize") 
                                    "Output as text file with reduction BlockSize"
    , Option "d"   ["diff"]     (ReqArg (\f o -> o { overDiffOverlays = Just f}) "FILENAME")
                                    "Put a diff overlay on top of the generated image."
    ]

loadArgs :: [String] -> IO OverOption 
loadArgs args =
    case getOpt Permute commonOption args of
         (_, _, lst@(_:_)) -> do
             mapM_ (hPutStrLn stderr) lst
             exitWith $ ExitFailure 2
                                  
         (opts, file, _) -> return $ 
             (foldr (\f opt -> f opt) defaultOption opts){ overFiles = file }

loadConf :: OverOption -> IO ColorDef
loadConf opts
    | null $ overConf opts = do
        when (overVerbose opts)
             (putStrLn "Using default colors")
        return defaultColorDef
    | otherwise = do
        file <- readFile $ overConf opts
        let colors = parseColorDef file
        when (overVerbose opts)
             (putStrLn "Using colors " >> print colors)
        return colors

loadErrorFile :: OverOption -> IO [(String, Int)]
loadErrorFile opts = case overErrFile opts  of
      Nothing -> return []
      Just fName -> do
        when (overVerbose opts)
             (putStrLn $ "Loading '" ++ fName ++ "' as error line definition")
        file <- readFile fName
        return [(msg, read $ tail lineNum) 
                        | (msg, lineNum) <- map (break (':' ==)) $ lines file ]

loadDiffOverlay :: OverOption -> IO CodeOverlays
loadDiffOverlay opts = case overDiffOverlays  opts of
    Nothing -> return []
    Just f -> loadOverlayFile f

savePngImage :: OverOption -> FilePath -> [[ViewColor]] -> IO ()
savePngImage option path pixels
    | overTransparent option = savePng24BitAlpha path pixels
    | otherwise = savePng24Bit path $ toRgb pixels
        where toRgb pixelsList =
                  [map (\(a,b,c,_) -> (a,b,c)) line | line <- pixelsList]


performTransformation :: OverOption -> FilePath -> IO [[ViewColor]]
performTransformation option path = do
    file <- B.readFile path
    codeDef <- parserOfFile option path
    colorDef <- loadConf option
    errorLines <- loadErrorFile option
    diffOverlay <- loadDiffOverlay option
    when (overVerbose option)
         (putStrLn $ "highlight List : " ++ show (overHighlighted option))
    let converter =
            if overHeatMap option
               then createHeatMap codeDef colorDef errorLines
               else fst . createCodeOverview codeDef colorDef errorLines 
                                            diffOverlay (overHighlighted option)
        pixelList = converter $ B.lines file
    if overTop option >= 0
       then return $ addOverMask colorDef (0, overTop option - 1)
                                          (5000, overHiSize option - 1)
                                          pixelList
       else return pixelList

printHelp :: IO ()
printHelp = putStrLn helpText
    where helpHeader = "codeOverview usage :\n"
                    ++ "  codeOverview [OPTIONS] files...\n"
                    ++ "\n"
                    ++ "Options :\n"
          helpText = usageInfo helpHeader commonOption 

codeDefOfExt :: OverOption -> String -> String -> IO (CodeDef [CodeEntity])
codeDefOfExt option@(OverOption { overFileFormat = Nothing }) path extension =
     maybe (return emptyCodeDef) 
           (\(name, code) -> do
               when (overVerbose option)
                    (putStrLn $ "Choosing " ++ name ++ " parser for " ++ path)
               return code)
         $ lookup extension extensionAssociation

codeDefOfExt option@(OverOption { overFileFormat = Just extension }) path ext =
     maybe (codeDefOfExt option path ext) return
         $ lookup extension formatParserAssociation 

parserOfFile :: OverOption -> FilePath -> IO (CodeDef [CodeEntity])
parserOfFile option path =
    let (fname, ext) = splitExtension path
        fileExt = if ext == "" then snd $ splitFileName fname
                               else ext
    in codeDefOfExt option path fileExt

createAscii :: OverOption -> IO ()
createAscii options = do
    forM_ (overFiles options) (\file ->
        do when (overVerbose options)
                (putStrLn $ "Processing " ++ file)
           pixels <- do
                codeDef <- parserOfFile options file
                fileContent <- B.readFile file
                let (Just size) = overAsciiVersion options
                return . createAsciiOverview codeDef size []
                       $ B.lines fileContent

           if null pixels
              then putStrLn "Error no pixel processed"
              else (do let outPath = overTextOut options file
                       when (overVerbose options)
                            (putStrLn $ "Writing file at" ++ outPath)
                       writeFile outPath $ unlines pixels))

createSingleFile :: OverOption -> IO ()
createSingleFile options = do
    mapM_ (\file ->
                do when (overVerbose options)
                        (putStrLn $ "Processing " ++ file)
                   pixels <- performTransformation options file
                   if null pixels
                      then putStrLn "Error no pixel processed"
                      else (do let outPath = overOut options file
                               when (overVerbose options)
                                    (putStrLn $ "Writing image at" ++ outPath)
                               savePngImage options outPath pixels)
                   ) $ overFiles options

createGraph :: OverOption -> IO ()
createGraph options = do
    colorDef <- loadConf options

    when (overVerbose options)
         (putStrLn "creating graph")
    currentDir <- getCurrentDirectory 

    fileList <- if overRecursiveDiscovery options
                   then listAllSourceFiles isSourceFile
                                           True currentDir 
                   else return $ overFiles options
    
    when (overVerbose options)
         (mapM_ (putStrLn . ("= found " ++)) fileList)

    createIncludeGraph parserForFile (overVerbose options)
                       colorDef "graph.dot"
                       (overIncludeDirs options) fileList

main :: IO ()
main = do
    options <- getArgs >>= loadArgs

    when (overHelp options)
         (printHelp >> exitWith ExitSuccess)

    when (null (overFiles options) &&
           not (overGraph options && overRecursiveDiscovery options))

         (hPutStrLn stderr "Error : no file input given (try --help for futher information)"
         >> exitWith (ExitFailure 1))

    when (overGraph options)
         (createGraph options >> exitWith ExitSuccess)

    when (overAsciiVersion options /= Nothing)
         (createAscii options >> exitWith ExitSuccess)

    createSingleFile options

