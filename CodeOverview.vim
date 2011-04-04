"=============================================================================
" What Is This: Launch an helper window to display overview of edited files.
" File: CodeOverview
" Author: Vincent B <twinside@gmail.com>
" Last Change: 2010 janv. 29
" Version: 2.0
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
"   let g:code_overview_autostart = 1
"       To start the plugin directly at vim launch.
"
"   let g:codeoverview_autoupdate = 1
"       To automaticly start automatic overview generation.
"       Otherwise you have to manually call SnapshotFile
"       to update the view.
"       (disabled by default)
"
"   let g:codeOverviewMaxLineCount = 10000
"       To avoid locking up vim, you can provide a maximum
"       line count to avoid refresh for very huges file.
"       default is 10000
"
" Thanks:
"  - Amjidanutpan Rama : forcing me to test the plugin
"		         under Windows XP.
" ChangeLog:
"     * 2.0  : Adding linoux Versioun
"     * 1.8  : Added option to start plugin automatically.
"     * 1.7  : Added condition to avoid loading very huge file.
"     * 1.6  : Handling of HUUUUUUGES file
"     * 1.5  : Update in stabilities for binaries.
"     * 1.4  : Big fix to allow maximization on Windows XP.
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
fun! ShowCodeOverviewParams() "{{{
    echo 's:tempDir ' . s:tempDir
    echo 's:rmCommand ' . s:rmCommand
    echo 's:tempCommandFile ' . s:tempCommandFile
    echo 's:initPid ' . s:initPid
    echo 's:wakeFile ' . s:wakeFile
    echo 's:tempFile ' . s:tempFile
    echo 's:friendProcess ' . s:friendProcess
    echo 's:overviewProcess ' . s:overviewProcess
endfunction "}}}

if exists("g:__CODEOVERVIEW_VIM__")
    finish
endif
let g:__CODEOVERVIEW_VIM__ = 1

if !has("gui_running")
    finish
endif

" 
if v:version < 703
    echo 'Your vim version is too old for the CodeOverview plugin, please update it'
    finish
endif

let s:preparedParameters = 0
fun! s:PrepareParameters() "{{{
	if s:preparedParameters
        return
    endif

    " Some version of vim don't get globpath with additional
    " flag to avoid wildignore, so we must do it by hand
    let s:tempWildIgnore = &wildignore
    set wildignore=

    if has("win32") || has("win64")
       let s:tempDir = expand("$TEMP") . '\'
       let s:rmCommand = "erase "
       let s:tempCommandFile = s:tempDir . 'command.cmd'
    else
       let s:tempDir = "/tmp/"
       let s:rmCommand = "rm "
       let s:tempCommandFile = s:tempDir . 'command.sh'
    endif

    let s:initPid = string(getpid())
    let s:wakeFile = s:tempDir . 'overviewFile' . s:initPid . '.txt'
    let s:tempFile = s:tempDir . 'previewer' . s:initPid . '.png'

    echo s:initPid
    execute 'set wildignore=' . s:tempWildIgnore

    let s:preparedParameters = 1
endfunction "}}}

fun! s:InitialInit() "{{{
    " Some version of vim don't get globpath with additional
    " flag to avoid wildignore, so we must do it by hand
    let s:tempWildIgnore = &wildignore
    set wildignore=

    if has("win32") || has("win64")
       let s:friendProcess = '"' . globpath( &rtp, 'plugin/WpfOverview.exe' ) . '"'
       let s:overviewProcess = '"' . globpath( &rtp, 'plugin/codeoverview.exe' ) . '"'
    else
       let s:friendProcess = '"' . globpath( &rtp, 'plugin/gtkOverview.py' ) . '"'
       let s:overviewProcess = '"' . globpath( &rtp, 'plugin/codeoverview' ) . '"'
    endif

    execute 'set wildignore=' . s:tempWildIgnore
endfunction "}}}

let s:friendProcessStarted = 0

if !exists("g:codeOverviewMaxLineCount")
    let g:codeOverviewMaxLineCount = 10000
endif

call s:InitialInit()

if s:friendProcess == '""' || s:overviewProcess == '""'
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

    call s:PrepareParameters()

    if has("win32")
        call system('cmd /s /c "start "CodeOverview Launcher" /b '
                \ . s:friendProcess . ' ' . s:initPid . '"')
    else
        let cmd = '!'. s:friendProcess . ' ' . s:initPid . ' &'
        exec cmd
    endif

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
    if line('$') > g:codeOverviewMaxLineCount
        echo 'File to big, no overview generated'
        return
    endif

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
    if has("win32")
        let wakeCommand = 'echo ' . string(winInfo.topline) 
                        \ . '?' . string(lastVisibleLine)
                        \ . '?' . s:tempFile . ' > "' . s:wakeFile . '"'
    else
        let wakeCommand = 'echo "' . string(winInfo.topline) 
                        \ . '?' . string(lastVisibleLine)
                        \ . '?' . s:tempFile . '" > "' . s:wakeFile . '"'
    endif

    if &modified
        call writefile( [commandLine, wakeCommand, s:rmCommand . filename], s:tempCommandFile )
    else
        call writefile( [commandLine, wakeCommand], s:tempCommandFile )
    endif

    if has("win32")
        call system( '"' . s:tempCommandFile . '"' )
    else 
    	call system( 'sh "' . s:tempCommandFile . '" &' )
    endif
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

if exists("g:code_overview_autostart")
	call s:LaunchFriendProcess()
endif

