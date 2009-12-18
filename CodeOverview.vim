"=============================================================================
" What Is This: Launch an helper window to display overview of edited files.
" File: CodeOverview
" Author: Vincent B <twinside@gmail.com>
" Last Change: 2009 déc. 18
" Version: 1.3.1
" Require:
"   * set nocompatible
"       somewhere on your .vimrc
"   * Running on Windows with .net >= 3.0
"   * Running gVim version 7.2 or greater (not vim)
"
" Usage:
"   :ShowCodeOverview 
"       Start the overview panel.
"
"   :HideCodeOverview 
"       Hide the overview panel, doesn't deactivate
"       Also stop automatic overview generation.
"
"   :CodeOverviewNoAuto 
"       Disable automatic overview generation.
"
"   :CodeOverviewAuto 
"       Setup automatic overview generation.
"
"   :SnapshotFile 
"       Refresh your current overview.
"
" Additional:
"   let g:codeoverview_autoupdate = 1
"       To automaticly start automatic overview generation.
"       Otherwise you have to manually call SnapshotFile
"       to update the view.
"       (disabled by default)
"
" Thanks:
"  - Amjidanutpan Rama : forcing me to test the plugin
"		         under Windows XP.
" ChangeLog:
"     * 1.3.1: Fixed problem of path under Windows XP
"	       Fixed problem of overview when file has no
"	       name
"     * 1.3  : Fixed problem when executables are put in
"              Program Files (yeah, I mentioned to put it
"              in ~/vimfiles, whatever...)
"     * 1.2  : Added check for the version of gvim.
"     * 1.1  : fixed problem with globpath flag for not cutting-edge
"              vim.
"     * 1.0  : Original version
"
if exists("g:__CODEOVERVIEW_VIM__")
    finish
endif
let g:__CODEOVERVIEW_VIM__ = 1

if !has("win32") || !has("gui_running")
    " Windows only plugin
    " only with GUI
    finish
endif

" 
if v:version < 702 || (v:version == 702 && !has('patch264'))
    echo 'Your vim version is too old for the CodeOverview plugin, please update it'
    echo 'last version avaible at http://sourceforge.net/projects/cream/files/Vim'
    finish
endif

let s:tempDir = expand("$TEMP") . '\'

" Some version of vim don't get globpath with additional
" flag to avoid wildignore, so we must do it by hand
let s:tempWildIgnore = &wildignore
set wildignore=
let s:friendProcess = '"' . globpath( &rtp, 'plugin/WpfOverview.exe' ) . '"'
let s:overviewProcess = '"' . globpath( &rtp, 'plugin/codeoverview.exe' ) . '"'
execute 'set wildignore=' . s:tempWildIgnore

let s:wakeFile = s:tempDir . 'overviewFile' . string(getpid()) . '.txt'
let s:tempFile = s:tempDir . 'previewer' . string(getpid()) . '.png'
let s:tempCommandFile = s:tempDir . 'command.cmd'
let s:friendProcessStarted = 0

if s:friendProcess == '' || s:overviewProcess == ''
    echo "Can't find friend executables, aborting CodeOverview load"
    finish
endif

au VimLeavePre * call s:RemoveTempsFile()

fun! s:RemoveTempsFile() "{{{
    call delete( s:wakeFile )
    call delete( s:tempFile )
    call delete( s:tempCommandFile )
endfunction "}}}

fun! s:StopFriendProcess() "{{{
    if s:friendProcessStarted == 0
        echo 'Friend process is already stopped'
        return
    endif

    call writefile( ["quit"], s:wakeFile )
    let s:friendProcessStarted = 0

    call s:RemoveCodeOverviewHook()

    command! CodeOverviewNoAuto echo 'CodeOverview Friend Process not started!'
    command! CodeOverviewAuto echo 'CodeOverview Friend Process not started!'
    command! SnapshotFile echo 'CodeOverview Friend Process not started!'
endfunction "}}}

" Launch the tracking window for this instance of gVIM
" Configure some script variables used in this script.
fun! s:LaunchFriendProcess() "{{{
    if s:friendProcessStarted == 1
        echo 'Friend process already started'
        return
    endif

    call system('cmd /s /c "start "CodeOverview Launcher" /b '
             \ . s:friendProcess . ' ' . string( getpid() ) . '"')
    let s:friendProcessStarted = 1

    if exists("g:codeoverview_autoupdate")
        call s:PutCodeOverviewHook()
    endif

    command! CodeOverviewNoAuto call s:RemoveCodeOverviewHook()
    command! CodeOverviewAuto call s:PutCodeOverviewHook()
    command! SnapshotFile call s:SnapshotFile()

    call s:SnapshotFile()
endfunction "}}}

" This fuction extract data from the current view,
" generate an overview image of the current file,
" write an in an update file readen by the following
" window.
fun! s:SnapshotFile() "{{{
    " If file has been modified, we must dump it somewhere
    if &modified
        let lines = getline( 0, line('$') )
        let filename = s:tempDir . 'tempVimFile' . expand( '%:t' )
        call writefile(lines, filename)
	let filename = '"' . filename . '"'
        let lines = [] " Just to let the garbage collector do it's job.
    else
        let filename = '"' . expand( '%' ) . '"'
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
    let commandLine = s:overviewProcess . ' -o "' . s:tempFile . '" '
                             \ . highlighted . " " . filename

    " Make an non-blocking start
    let wakeCommand = 'echo ' . string(winInfo.topline) 
                      \ . '?' . string(lastVisibleLine)
                      \ . '?' . s:tempFile . ' > "' . s:wakeFile . '"'

    if &modified
        call writefile( [commandLine, wakeCommand, 'erase ' . filename], s:tempCommandFile )
    else
        call writefile( [commandLine, wakeCommand], s:tempCommandFile )
    endif

    call system( '"' . s:tempCommandFile . '"' )
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

fun! s:RemoveCodeOverviewHook() "{{{
    augroup CodeOverview
        au!
    augroup END
endfunction "}}}

command! CodeOverviewNoAuto echo 'CodeOverview Friend Process not started!'
command! CodeOverviewAuto echo 'CodeOverview Friend Process not started!'
command! SnapshotFile echo 'CodeOverview Friend Process not started!'
command! ShowCodeOverview call s:LaunchFriendProcess()
command! HideCodeOverview call s:StopFriendProcess()

