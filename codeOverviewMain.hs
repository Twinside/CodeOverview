import CodeOverviewGenerator.CodeOverview
import Png

import Control.Monad( when )

import System.IO
import System.Exit
import System.Environment
import System.FilePath

import System.Console.GetOpt

data OverOption = OverOption
      { overOut :: String -> String
      , overVersion :: Bool
      , overConf :: String
      , overHelp :: Bool
      , overTransparent :: Bool
      , overVerbose :: Bool
      , overTop :: Int
      , overHiSize :: Int
      , overFiles :: [String]
      , overHighlighted :: [String]
      , overErrFile :: Maybe String
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
    }

pngIzeExtension :: FilePath -> FilePath
pngIzeExtension = (++ ".png") . fst . splitExtension

commonOption :: [OptDescr (OverOption -> OverOption)]
commonOption =
    [ 
      Option ['a'] ["alpha"]   (NoArg (\o -> o{overTransparent = True}))
                               "Produce a png with transparency"
    , Option ['h'] ["help"]    (NoArg (\o -> o{ overHelp = True}))
                               "Display inline help"
    , Option ['o'] ["output"]  (ReqArg (\f o -> o{ overOut = const f}) "FILE")
                               "Output FILE, by default same name with extension replaced."
    , Option ['c'] ["conf"]    (ReqArg (\f o -> o{ overConf = f}) "FILECONF")
                               "Configuration file, to configure colors."
    , Option []    ["vs"]      (ReqArg (\f o -> o{overHiSize=read f}) "Size")
                               "Viewport size (in lines)."
    , Option []    ["hi"]      (ReqArg (\f o -> o{overHighlighted = f:overHighlighted o}) "Highlight")
                               "Highlight a word"
    , Option ['t'] ["top"]     (ReqArg (\f o -> o{overTop=read f}) "TOP")
                               "Viewport beginning line"
    , Option ['v'] ["verbose"] (NoArg (\o -> o{overVerbose = True}))
                               "Show progression and various information"
    , Option ['V'] ["version"] (NoArg (\o -> o{overVersion = True}))
                               "Show version number and various information"
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
             
extensionAssociation :: [(String, (String, CodeDef))]
extensionAssociation =
    [ (".hs"    , ("haskell"      , haskellCodeDef))
    , (".c"     , ("C"            , cCodeDef))
    , (".h"     , ("C/C++ Header" , cCodeDef))
    , (".C"     , ("C++"          , cCodeDef))
    , (".cs"    , ("C#"           , cCodeDef))
    , (".cpp"   , ("C++"          , cCodeDef))
    , (".cc"    , ("C++"          , cCodeDef))
    , (".java"  , ("Java"         , cCodeDef))
    , (".js"    , ("Javascript"   , cCodeDef))
    , (".m"     , ("Objective C"  , cCodeDef))
    , (".ml"    , ("OCaml"        , ocamlCodeDef))
    , (".mli"   , ("OCaml"        , ocamlCodeDef))
    , (".fs"    , ("F#"           , ocamlCodeDef))
    , (".fsi"   , ("F#"           , ocamlCodeDef))
    , (".py"    , ("Python"       , pythonCodeDef))
    , (".sh"    , ("Shell Script" , shellLikeCodeDef))
    , ("Makefile", ("Shell Script" , shellLikeCodeDef))
    , (".rb"    , ("Rubyt"        , rubyCodeDef))
    , (".html"  , ("HTML"         , htmlCodeDef))
    , (".htm"   , ("HTML"         , htmlCodeDef))
    , (".xml"   , ("XML"          , htmlCodeDef))
    , (".xhtml" , ("xHTML"        , htmlCodeDef))
    ]

loadConf :: OverOption -> IO ColorDef
loadConf opts
    | null $ overConf opts = return defaultColorDef
    | otherwise = do
        file <- readFile $ overConf opts
        return $ parseColorDef file

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

codeDefOfExt :: OverOption -> String -> String -> IO CodeDef
codeDefOfExt option path extension =
    maybe (return emptyCodeDef) 
          (\(name, code) -> do
              when (overVerbose option)
                   (putStrLn $ "Choosing " ++ name ++ " parser for " ++ path)
              return code)
        $ lookup extension extensionAssociation

performTransformation :: OverOption -> FilePath -> IO [[ViewColor]]
performTransformation option path = do
    file <- readFile path
    let (fname, ext) = splitExtension path
        fileExt = if ext == "" then snd $ splitFileName fname
                               else ext
    codeDef <- codeDefOfExt option path fileExt
    colorDef <- loadConf option
    errorLines <- loadErrorFile option
    let pixelList = createCodeOverview 
                        codeDef
                        colorDef
                        errorLines
                        (overHighlighted option)
                        $ lines file
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

main :: IO ()
main = do
    options <- getArgs >>= loadArgs
    when (overHelp options)
         (printHelp >> exitWith ExitSuccess)
    when (null $ overFiles options)
         (hPutStrLn stderr "Error : no file input given (try --help for futher information)"
         >> exitWith (ExitFailure 1))

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

