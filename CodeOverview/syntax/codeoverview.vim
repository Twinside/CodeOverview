syn clear

if exists("b:current_syntax")
  finish
endif

syn match overComment      /a/
syn match overString       /b/
syn match overNormal       /c/
syn match overHighlight    /d/
syn match overMaj          /e/
syn match overEmpty        /f/
syn match overView         /g/
syn match overKeyword      /h/
syn match overType         /i/
syn match overLabel        /j/
syn match overConditional  /k/
syn match overRepeat       /l/
syn match overStructure    /m/
syn match overStatement    /n/
syn match overPreproc      /o/
syn match overMacro        /p/
syn match overTypedef      /q/
syn match overException    /r/
syn match overOperator     /s/
syn match overInclude      /t/
syn match overStorageClass /u/
syn match overChar         /v/
syn match overNumber       /w/
syn match overFloat        /x/
syn match overBool         /y/
syn match overFunction     /z/

syn match overTag          /0/
syn match overAttribTag    /1/
syn match overError        /2/
syn match overWarning      /3/
syn match overInfo         /4/


hi link overComment        Comment
hi link overString         String
hi link overNormal         Normal
hi link overHighlight      Highlight
hi link overMaj            Normal
hi link overEmpty          Normal
hi link overView           View
hi link overKeyword        Keyword
hi link overType           Type
hi link overLabel          Label
hi link overConditional    Conditional
hi link overRepeat         Repeat
hi link overStructure      Structure
hi link overStatement      Statement
hi link overPreproc        Preproc
hi link overMacro          Macro
hi link overTypedef        Typedef
hi link overException      Exception
hi link overOperator       Operator
hi link overInclude        Include
hi link overStorageClass   StorageClass
hi link overChar           Char
hi link overNumber         Number
hi link overFloat          Float
hi link overBool           Bool
hi link overFunction       Function
hi link overTag            Tag
hi link overAttribTag      AttribTag
hi link overError          Error
hi link overWarning        Warning
hi link overInfo           Info

let b:current_syntax = "codeoverview"

