-- | Module defining the VimL language
module CodeOverviewGenerator.Language.Vim ( vimCodeDef) where

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
{-import qualified CodeOverviewGenerator.ByteString as B-}

-- | VimL tokens.
vimConditional, vimRepeat, vimBuiltin, vimStatement :: [String]
vimConditional = ["if", "else", "elseif", "endif", "finish"]

vimStatement = ["let", "unlet", "exe", "hi", "syn"]
vimRepeat = ["while", "for", "do"]

vimBuiltin = [
 "abs", "append", "argv", "atan2", "bufexists",
 "bufname", "byte2line", "ceil", "cindent", "complete",
 "confirm", "cosh", "cursor", "did_filetype", "empty",
 "eventhandler", "exp", "extend", "filewritable", "findfile",
 "fmod", "foldclosed", "foldtext", "function", "getbufline",
 "getcharmod", "getcmdtype", "getfperm", "getftype", "getmatches",
 "getqflist", "gettabvar", "getwinposy", "globpath", "haslocaldir",
 "histdel", "hlexists", "iconv", "input", "inputrestore",
 "insert", "items", "len", "line", "localtime",
 "map", "match", "matchdelete", "matchstr", "min",
 "mode", "nextnonblank", "pathshorten", "prevnonblank", "pumvisible",
 "readfile", "reltimestr", "remote_foreground", "remote_read", "remove",
 "repeat", "reverse", "search", "searchpair", "searchpos",
 "serverlist", "setcmdpos", "setloclist", "setpos", "setreg",
 "settabwinvar", "shellescape", "sin", "sort", "spellbadword",
 "split", "str2float", "strchars", "strftime", "string",
 "strpart", "strtrans", "submatch", "synconcealed", "synIDattr",
 "synstack", "tabpagebuflist", "tabpagewinnr", "taglist", "tanh",
 "tolower", "tr", "type", "undotree", "virtcol",
 "winbufnr", "winheight", "winnr", "winrestview", "winwidth",
 "acos", "argc", "asin", "browse", "buflisted",
 "bufnr", "byteidx", "changenr", "clearmatches", "complete_add",
 "copy", "count", "deepcopy", "diff_filler", "escape",
 "executable", "expand", "feedkeys", "filter", "float2nr",
 "fnameescape", "foldclosedend", "foldtextresult", "garbagecollect", "getbufvar",
 "getcmdline", "getcwd", "getfsize", "getline", "getpid",
 "getreg", "gettabwinvar", "getwinvar", "has", "hasmapto",
 "histget", "hlID", "indent", "inputdialog", "inputsave",
 "isdirectory", "join", "libcall", "line2byte", "log",
 "maparg", "matchadd", "matchend", "max", "mkdir",
 "mzeval", "nr2char", "pow", "printf", "range",
 "reltime", "remote_expr", "remote_peek", "remote_send", "rename",
 "resolve", "round", "searchdecl", "searchpairpos", "server2client",
 "setbufvar", "setline", "setmatches", "setqflist", "settabvar",
 "setwinvar", "simplify", "sinh", "soundfold", "spellsuggest",
 "sqrt", "str2nr", "strdisplaywidth", "stridx", "strlen",
 "strridx", "strwidth", "substitute", "synID", "synIDtrans",
 "system", "tabpagenr", "tagfiles", "tan", "tempname",
 "toupper", "trunc", "undofile", "values", "visualmode",
 "wincol", "winline", "winrestcmd", "winsaveview", "writefile",
 "add", "argidx", "atan", "browsedir", "bufloaded",
 "bufwinnr", "call", "char2nr", "col", "complete_check",
 "cos", "cscope_connection", "delete", "diff_hlID", "eval",
 "exists", "expr8", "filereadable", "finddir", "floor",
 "fnamemodify", "foldlevel", "foreground", "get", "getchar",
 "getcmdpos", "getfontname", "getftime", "getloclist", "getpos",
 "getregtype", "getwinposx", "glob", "has_key", "histadd",
 "histnr", "hostname", "index", "inputlist", "inputsecret",
 "islocked", "keys", "libcallnr", "lispindent", "log10",
 "mapcheck", "matcharg", "matchlist"]


-- | VimL language definition.
vimCodeDef :: CodeDef [CodeEntity]
vimCodeDef = def
    where def = CodeDef
           { lineComm = strComment "\""
           , multiLineCommBeg = Nothing
           , multiLineCommEnd = Nothing
           , recursiveComment = False
           , tabSpace = 4
           , identParser = identWithPrime
           , strParser = Just $ stringParser False def
           , specialIdentifier = prepareKeywords
 {-, (cStatement, statementColor)-}
                [ (vimRepeat, RepeatEntity)
                , (vimBuiltin, FunctionEntity)
                , (vimConditional, ConditionalEntity)
                , (vimStatement, StatementEntity)
                {-, (cStructure, structureColor)-}
                {-, (cStorageClass, storageClassColor)-}
                ]
           , specificParser = [intParser]
           , heatTokens = []
           }

