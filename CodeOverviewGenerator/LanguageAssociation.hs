module CodeOverviewGenerator.LanguageAssociation where

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


parserForFile :: FilePath -> ColorDef -> CodeDef [ViewColor]
parserForFile path = maybe (const emptyCodeDef) snd
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

extensionAssociation :: [(String, (String, ColorDef -> CodeDef [ViewColor]))]
extensionAssociation = sourceExtensionAssoc ++ tagExtensionAssoc

sourceExtensionAssoc :: [(String, (String, ColorDef -> CodeDef [ViewColor]))]
sourceExtensionAssoc =
    [ (".hs"    , ("haskell"      , haskellCodeDef))
    , (".c"     , ("C"            , cCodeDef))
    , (".h"     , ("C/C++ Header" , cppCodeDef))
    , (".hpp"   , ("C++ Header"   , cppCodeDef))
    , (".C"     , ("C++"          , cppCodeDef))
    , (".cs"    , ("C#"           , cppCodeDef))
    , (".cpp"   , ("C++"          , cppCodeDef))
    , (".cc"    , ("C++"          , cppCodeDef))
    , (".java"  , ("Java"         , javaCodeDef))
    , (".js"    , ("Javascript"   , cppCodeDef))
    , (".m"     , ("Objective C"  , cppCodeDef))
    , (".ml"    , ("OCaml"        , ocamlCodeDef))
    , (".mli"   , ("OCaml"        , ocamlCodeDef))
    , (".fs"    , ("F#"           , ocamlCodeDef))
    , (".fsi"   , ("F#"           , ocamlCodeDef))
    , (".py"    , ("Python"       , pythonCodeDef))
    , (".sh"    , ("Shell Script" , shellCodeDef))
    , ("Makefile", ("Shell Script", shellCodeDef))
    , (".rb"    , ("Ruby"         , rubyCodeDef))
    ]

tagExtensionAssoc :: [(String, (String, ColorDef -> CodeDef [ViewColor]))]
tagExtensionAssoc =
    [ (".html"  , ("HTML"         , htmlCodeDef))
    , (".htm"   , ("HTML"         , htmlCodeDef))
    , (".xml"   , ("XML"          , htmlCodeDef))
    , (".xhtml" , ("xHTML"        , htmlCodeDef))
    ]
