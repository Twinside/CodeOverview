-- | Module defining haskell code generation
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


importParser :: Parser [CodeEntity]
importParser = cleanDef <$>
    token "import" <*> (spaceList <$> eatWhiteSpace 4)
                   <*> (token "qualified" <|> nullParser)
                   <*> (spaceList <$> eatWhiteSpace 4)
                   <*> (notChars " \t" >>= extractInfos)
      where spaceList n = replicate n EmptyEntity

            extractInfos s = Parser $ \rest -> do
               let path = foldl1 (</>) . map B.unpack $ B.split '.' s
               addIncludeFile . LocalInclude $ path <.> "hs"
               addIncludeFile . LocalInclude $ path <.> "lhs"
               addIncludeFile . LocalInclude $ path <.> "hs-boot"
               return $ Result (B.length s, rest)

            cleanDef impLength spaceList1 qualifiedLength
                     spaceList2 impSize =
                replicate impLength IncludeEntity
                    ++ spaceList1
                    ++ replicate qualifiedLength IncludeEntity
                    ++ spaceList2
                    ++ replicate impSize NormalEntity
                                            

              
-- | Haskell code definition.
haskellCodeDef :: CodeDef [CodeEntity]
haskellCodeDef = def
    where def = CodeDef
                { lineComm = strComment "--"
                , multiLineCommBeg = strComment "{-"
                , multiLineCommEnd = strComment "-}"
                , recursiveComment = True
                , tabSpace = 4
                , identParser = identWithPrime
                , strParser = Just $ stringParser False def
                , specialIdentifier = prepareKeywords
                    [ (hModule ++ hStructure, StringEntity)
                    , (hImport, IncludeEntity)
                    , (hTypedef, TypedefEntity)
                    , (hStatement, StatementEntity)
                    , (hConditional, ConditionalEntity)
                    , (hTypes, TypeEntity)
                    ]

                , specificParser = [ intParser, importParser]
                , heatTokens = []
                }

