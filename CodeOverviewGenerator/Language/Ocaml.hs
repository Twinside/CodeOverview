-- | Module for an OCaml parser
module CodeOverviewGenerator.Language.Ocaml ( ocamlCodeDef ) where

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color

-- | Ocaml keywords/token.
ocamlKeyword, ocamlTypes, ocamlOperator :: [String]
ocamlKeyword = [ "object","struct","sig","end", "include", "function", "do"
               , "value", "in", "inherit", "initializer", "land", "lazy"
               , "let", "match", "method", "mutable", "new", "of", "parser"
               , "private", "raise", "rec", "try", "type", "val", "virtual"
               , "when", "while", "with", "and", "as", "assert", "class"
               , "constraint", "else", "exception", "external", "fun"]

ocamlTypes =
        [ "array", "bool", "char", "exn", "float", "format", "format4"
        , "int", "int32", "int64", "lazy_t", "list", "nativeint", "option"
        , "string", "unit" ]

ocamlOperator = ["asr", "lor", "lsl", "lsr", "lxor", "mod", "not"]

-- | OCaml language definition.
ocamlCodeDef :: CodeDef [CodeEntity]
ocamlCodeDef = def
    where def = CodeDef
               { lineComm = Nothing
               , multiLineCommBeg = strComment "(*"
               , multiLineCommEnd = strComment "*)"
               , recursiveComment = True
               , tabSpace = 4
               , identParser = basicIdent
               , strParser = Just $ stringParser False def
               , specialIdentifier = prepareKeywords
                    [ (ocamlKeyword, KeywordEntity)
                    , (ocamlTypes, TypeEntity)
                    , (ocamlOperator, OperatorEntity)
                    ]
               , specificParser = []
               , heatTokens = []
               }

