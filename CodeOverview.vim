"if exists("g:__CODEOVERVIEW_VIM__")
    "finish
"endif
"let g:__CODEOVERVIEW_VIM__ = 1

let s:tempDir = expand("$TEMP") . '\'
let s:wakeFile = s:tempDir . 'overviewFile.txt'
let s:tempFile = s:tempDir . 'previewer.png'

fun! s:SnapshotFile() "{{{
    let filename = expand( '%' )
    let pid = getpid()

    " Generate the new image file
    let rez = system("codeoverview -o " . s:tempFile . " " . filename)
    echo rez
    call writefile( [string( pid ) . '|' . s:tempFile], s:wakeFile )
endfunction "}}}

command! SnapshotFile call s:SnapshotFile()

