"if exists("g:__CODEOVERVIEW_VIM__")
    "finish
"endif
"let g:__CODEOVERVIEW_VIM__ = 1

if !has("win32")
    " Windows only plugin
    finish
endif

let s:tempDir = expand("$TEMP") . '\'

fun! s:LaunchFriendProcess() "{{{
    let pid = getpid()
    "call system("cmd /c start WpfOverview.exe " . pid)
    let s:wakeFile = s:tempDir . 'overviewFile' . string(pid) . '.txt'
    echom s:wakeFile
    let s:tempFile = s:tempDir . 'previewer' . string(pid) . '.png'
endfunction "}}}

" This fuction extract data from the current view,
" generate an overview image of the current file,
" write an in an update file readen by the following
" window.
fun! s:SnapshotFile() "{{{
    " If file has been modified, we must dump it somewhere
    if &modified
        let lines = getline( 0, getline('$') )
        let filename = s:tempDir . expand( '%:t' )
        call writefile(lines, filename)
        let lines = [] " Just to let the garbage collector do it's job.
    else
        let filename = expand( '%' )
    endif

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
    " Make an non-blocking start
    let wakeCommand = 'echo ' . s:tempFile . ' > ' . s:wakeFile
    if &modified
        call writefile( [commandLine, wakeCommand, 'erase ' . filename], s:tempDir . 'command.cmd' )
    else
        call writefile( [commandLine, wakeCommand], s:tempDir . 'command.cmd' )
    endif
    call system( s:tempDir . 'command.cmd' )


    " Writing this file should update the display...
    " call writefile( [s:tempFile], s:wakeFile )
endfunction "}}}

fun! s:PutCodeOverviewHook() "{{{
    augroup CodeOverview
        au BufNewFile * call s:SnapshotFile()
        au BufEnter * call s:SnapshotFile()
        au BufNew * call s:SnapshotFile()
        au BufWritePost * call s:SnapshotFile()
        au FilterWritePost * call s:SnapshotFile()
        au StdinReadPost * call s:SnapshotFile()
        au FileChangedShellPost * call s:SnapshotFile()
    augroup END
endfunction "}}}

augroup CodeOverview
    au!
augroup END

"call s:LaunchFriendProcess()
call s:PutCodeOverviewHook()
"call s:SnapshotFile()

command! SnapshotFile call s:SnapshotFile()

