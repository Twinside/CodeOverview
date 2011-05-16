module CodeOverviewGenerator.Language.Haskell ( haskellCodeDef ) where

import Control.Applicative
import System.FilePath

import qualified CodeOverviewGenerator.ByteString as B
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

hStructure, hImport, hTypedef, hStatement, hConditional,
    hTypes, hModule :: [String]
hModule = ["module"]
hStructure = ["class", "data", "deriving", "instance", "default", "where"]
hImport = ["import"]
hTypedef = ["type", "newtype"]
hStatement = ["do", "case", "of", "let", "in"]
hConditional = ["if", "then", "else"]
hTypes = ["Int", "Integer", "Char", "Bool", "Float"
         , "Double", "IO", "Void", "Addr", "Array"
         , "String", "Maybe", "Either", "Ratio", "Complex"
         , "Ordering", "IOError", "IOResult", "ExitCode"]


importParser :: ColorDef -> Parser [ViewColor]
importParser colors = cleanDef <$>
    token "import" <*> (spaceList <$> eatWhiteSpace 4)
                   <*> (token "qualified" <|> nullParser)
                   <*> (spaceList <$> eatWhiteSpace 4)
                   <*> (notChars " \t" >>= extractInfos)
      where spaceList n = replicate n spaceColor
            spaceColor = emptyColor colors
            impColor = includeColor colors

            extractInfos s = Parser $ \rest -> do
               let path = foldl1 (</>) . map B.unpack $ B.split '.' s
               addIncludeFile . LocalInclude $ path <.> "hs"
               addIncludeFile . LocalInclude $ path <.> "lhs"
               addIncludeFile . LocalInclude $ path <.> "hs-boot"
               return $ Result (B.length s, rest)

            cleanDef impLength spaceList1 qualifiedLength
                     spaceList2 impSize =
                replicate impLength impColor
                    ++ spaceList1
                    ++ replicate qualifiedLength impColor
                    ++ spaceList2
                    ++ replicate impSize (normalColor colors)
                                            

              
                
haskellCodeDef :: ColorDef -> CodeDef [ViewColor]
haskellCodeDef colors = def
    where def = CodeDef
                { lineComm = strComment "--"
                , multiLineCommBeg = strComment "{-"
                , multiLineCommEnd = strComment "-}"
                , recursiveComment = True
                , tabSpace = 4
                , identParser = identWithPrime
                , strParser = Just $ stringParser False def
                , specialIdentifier = prepareKeywords colors
                    [ (hModule ++ hStructure, structureColor)
                    , (hImport, includeColor)
                    , (hTypedef, typedefColor)
                    , (hStatement, statementColor)
                    , (hConditional, conditionalColor)
                    , (hTypes, typeColor)
                    ]

                , specificParser = [ intParser colors, importParser colors]
                }

