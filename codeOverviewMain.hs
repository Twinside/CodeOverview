import System.Environment
import CodeOverview
import Png
import Codec.Image.PPM

main :: IO ()
main = do
    args <- getArgs
    putStrLn "======================================="
    file <- readFile $ args !! 0
    let pixelList = createCodeOverview
                        haskellCodeDef
                        defaultColorDef 
                        $ lines file
        rgbPixels = [map (\(a,b,c,d) -> (a,b,c)) line | line <- pixelList]
    putStrLn "======================================="
    savePng24BitAlpha (args !! 0 ++ ".rgba.png") pixelList
    savePng24Bit (args !! 0 ++ ".rgb.png") rgbPixels
    writeFile (args !! 0 ++ ".ppm") $ ppm rgbPixels
    
