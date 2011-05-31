import Control.Monad( when )
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

data OverOption = OverOption
      { overOut :: String -> String
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
      }

defaultOption :: OverOption
defaultOption = OverOption
    { overOut = pngIzeExtension
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
    }

pngIzeExtension :: FilePath -> FilePath
pngIzeExtension = (++ ".png") . fst . splitExtension

commonOption :: [OptDescr (OverOption -> OverOption)]
commonOption =
    [ 
      Option "a" ["alpha"]   (NoArg (\o -> o{overTransparent = True}))
                               "Produce a png with transparency"
    , Option "h" ["help"]    (NoArg (\o -> o{ overHelp = True}))
                               "Display inline help"
    , Option "o" ["output"]  (ReqArg (\f o -> o{ overOut = const f}) "FILE")
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
    , Option []    ["graph"]   (NoArg (\o -> o{overGraph = True}))
                               "Create a graph from a bunch of code sources."
    , Option "r"   ["recursive-discovery"] (NoArg (\o -> o{ overRecursiveDiscovery = True }))
                               "Try to find source file for graph drawing automatically."
    , Option "I" ["include-dir"] (ReqArg (\f o -> o { overIncludeDirs = overIncludeDirs o ++ [f]}) "Directory")
                                   "Add a directory to search for include files."
    , Option []    ["errfile"]  (ReqArg (\f o -> o {overErrFile = Just f}) "FILENAME") "Error lines"
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
    when (overVerbose option)
         (putStrLn $ "highlight List : " ++ show (overHighlighted option))
    let (pixelList, _) = createCodeOverview 
                            (codeDef colorDef)
                            colorDef
                            errorLines
                            (overHighlighted option)
                            $ B.lines file
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

codeDefOfExt :: OverOption -> String -> String -> IO (ColorDef -> CodeDef [ViewColor])
codeDefOfExt option@(OverOption { overFileFormat = Nothing }) path extension =
     maybe (return $ const emptyCodeDef) 
           (\(name, code) -> do
               when (overVerbose option)
                    (putStrLn $ "Choosing " ++ name ++ " parser for " ++ path)
               return code)
         $ lookup extension extensionAssociation
codeDefOfExt option@(OverOption { overFileFormat = Just extension }) path ext =
     maybe (codeDefOfExt option path ext) return
         $ lookup extension formatParserAssociation 

parserOfFile :: OverOption -> FilePath -> IO (ColorDef -> CodeDef [ViewColor])
parserOfFile option path =
    let (fname, ext) = splitExtension path
        fileExt = if ext == "" then snd $ splitFileName fname
                               else ext
    in codeDefOfExt option path fileExt

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
    if overGraph options
       then createGraph options
       else createSingleFile options

