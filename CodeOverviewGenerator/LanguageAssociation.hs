module CodeOverviewGenerator.LanguageAssociation(
              parserForFile
            , isSourceFile
            , extensionAssociation
            , formatParserAssociation
            , sourceExtensionAssoc
            , tagExtensionAssoc ) where

import System.FilePath

import CodeOverviewGenerator.CodeOverview

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Language.C
import CodeOverviewGenerator.Language.Cpp
import CodeOverviewGenerator.Language.Java
import CodeOverviewGenerator.Language.Ocaml
import CodeOverviewGenerator.Language.Haskell
import CodeOverviewGenerator.Language.Python
import CodeOverviewGenerator.Language.Shell
import CodeOverviewGenerator.Language.Ruby
import CodeOverviewGenerator.Language.Html
import CodeOverviewGenerator.Language.Vim


parserForFile :: FilePath -> CodeDef [CodeEntity]
parserForFile path = maybe emptyCodeDef snd
                   $ fileExt `lookup` extensionAssociation
    where (fname, ext) = splitExtension path
          fileExt = if ext == "" then snd $ splitFileName fname
                                 else ext
                    
isSourceFile :: FilePath -> Bool
isSourceFile path = notNothing $ fileExt `lookup` sourceExtensionAssoc
    where (fname, ext) = splitExtension path
          fileExt = if ext == "" then snd $ splitFileName fname
                                 else ext
          notNothing Nothing = False
          notNothing _ = True

extensionAssociation :: [(String, (String, CodeDef [CodeEntity]))]
extensionAssociation = sourceExtensionAssoc ++ tagExtensionAssoc

formatParserAssociation :: [(String, CodeDef [CodeEntity])]
formatParserAssociation =
    [ ("haskell", haskellCodeDef)
    , ("c", cCodeDef)
    , ("cpp", cppCodeDef)
    , ("java", javaCodeDef)
    , ("ocaml", ocamlCodeDef)
    , ("python", pythonCodeDef)
    , ("ruby", rubyCodeDef)
    , ("html", htmlCodeDef)
    , ("xml", htmlCodeDef)
    ]

sourceExtensionAssoc :: [(String, (String, CodeDef [CodeEntity]))]
sourceExtensionAssoc =
    [ (".hs"    , ("haskell"      , haskellCodeDef))
    , (".c"     , ("C"            , cCodeDef))
    , (".h"     , ("C/C++ Header" , cppCodeDef))
    , (".hpp"   , ("C++ Header"   , cppCodeDef))
    , (".C"     , ("C++"          , cppCodeDef))
    , (".cs"    , ("C#"           , cppCodeDef))
    , (".cpp"   , ("C++"          , cppCodeDef))
    , (".cc"    , ("C++"          , cppCodeDef))
    , (".fx"    , ("HLSL shader"  , cppCodeDef))
    , (".vshad" , ("GLSL shader"  , cppCodeDef))
    , (".fshad" , ("GLSL shader"  , cppCodeDef))
    , (".cg"    , ("Cg shader"    , cppCodeDef))
    , (".java"  , ("Java"         , javaCodeDef))
    , (".js"    , ("Javascript"   , cppCodeDef))
    , (".as"    , ("ActionScript" , cppCodeDef))
    , (".m"     , ("Objective C"  , cppCodeDef))
    , (".ml"    , ("OCaml"        , ocamlCodeDef))
    , (".mli"   , ("OCaml"        , ocamlCodeDef))
    , (".fs"    , ("F#"           , ocamlCodeDef))
    , (".fsi"   , ("F#"           , ocamlCodeDef))
    , (".py"    , ("Python"       , pythonCodeDef))
    , (".sh"    , ("Shell Script" , shellCodeDef))
    , ("Makefile", ("Shell Script", shellCodeDef))
    , (".rb"    , ("Ruby"         , rubyCodeDef))
    , (".vim"    , ("VimL"         , vimCodeDef))
    ]

tagExtensionAssoc :: [(String, (String, CodeDef [CodeEntity]))]
tagExtensionAssoc =
    [ (".html"  , ("HTML"         , htmlCodeDef))
    , (".htm"   , ("HTML"         , htmlCodeDef))
    , (".xml"   , ("XML"          , htmlCodeDef))
    , (".mxml"  , ("Actionscript mxml", htmlCodeDef))
    , (".xhtml" , ("xHTML"        , htmlCodeDef))
    ]

