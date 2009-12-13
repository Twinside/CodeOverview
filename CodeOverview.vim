"if exists("g:__CODEOVERVIEW_VIM__")
    "finish
"endif
"let g:__CODEOVERVIEW_VIM__ = 1

let s:tempDir = expand("$TEMP") . '\'
let s:wakeFile = s:tempDir . 'overviewFile.txt'
let s:tempFile = s:tempDir . 'previewer.png'

" This fuction extract data from the current view,
" generate an overview image of the current file,
" write an in an update file readen by the following
" window.
fun! s:SnapshotFile() "{{{
    let filename = expand( '%' )
    let pid = getpid()

    let lastVisibleLine = line('w$')
    let winInfo = winsaveview()
    let research = getreg('/')

    " If we search an identifier
    if research =~ '\\<.*\\>'
        " Add a switch to let the image generator make it.
        let highlighted = ' --hi ' . substitute( research, '\\<\(.*\)\\>', '\1', '' )
    else
        let highlighted = ''
    endif

    " Generate the new image file
    let commandLine = "codeoverview -o " . s:tempFile
                             \ . " -t " . string(winInfo.topline)
                             \ . " --vs=" . string(lastVisibleLine - winInfo.topline)
                             \ . highlighted
                             \ . " " . filename

    call writefile( [commandLine], 'command.txt' )
    call system(commandLine)
    " Writing the file should update the display...
    call writefile( [string( pid ) . '|' . s:tempFile], s:wakeFile )
endfunction "}}}

command! SnapshotFile call s:SnapshotFile()

