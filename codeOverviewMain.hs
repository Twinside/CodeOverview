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
        finalpixelList = addOverMask defaultColorDef (5, 13) (30, 40) pixelList
        rgbPixels = [map (\(a,b,c,_) -> (a,b,c)) line | line <- finalpixelList]
    putStrLn "======================================="
    savePng24BitAlpha (args !! 0 ++ ".rgba.png") finalpixelList
    savePng24Bit (args !! 0 ++ ".rgb.png") rgbPixels
    writeFile (args !! 0 ++ ".ppm") $ ppm rgbPixels
    
