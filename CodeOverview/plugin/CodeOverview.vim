"=============================================================================
" What Is This: Launch an helper window to display overview of edited files.
" File: CodeOverview
" Author: Vincent B <twinside@gmail.com>
" Last Change: 2011 Apr 20
" Version: 2.0
if exists("g:__CODEOVERVIEW_VIM__")
    finish
endif
let g:__CODEOVERVIEW_VIM__ = 1

if !has("gui_running")
    finish
endif

if v:version < 702
    echo 'Your vim version is too old for the CodeOverview plugin, please update it'
    finish
endif


if !exists('g:codeOverviewShowErrorLines')
    let g:codeOverviewShowErrorLines = 1
endif

if !exists("g:code_overview_use_colorscheme")
	let g:code_overview_use_colorscheme = 1
endif

if !exists("g:codeOverviewMaxLineCount")
    let g:codeOverviewMaxLineCount = 10000
endif

let s:preparedParameters = 0
let s:friendProcessStarted = 0
let s:lastQuickfixKind = ''

if !exists("g:code_overview_ignore_buffer_list")
	let g:code_overview_ignore_buffer_list = []
endif

let s:builtinIgnoreList = [
            \ "__Tag_List__",
            \ "_NERD_tree_",
            \ "NERD_tree_1",
            \ "NERD_tree_2",
            \ "NERD_tree_3",
            \ "Source_Explorer",
            \ "[BuffExplorer]",
            \ "Todo List",
            \ "HoogleSearch"
            \ ]

let g:code_overview_ignore_buffer_list =
        \ g:code_overview_ignore_buffer_list + s:builtinIgnoreList

fun! ShowCodeOverviewParams() "{{{
    echo 's:tempDir ' . s:tempDir
    echo 's:rmCommand ' . s:rmCommand
    echo 's:tempCommandFile ' . s:tempCommandFile
    echo 's:initPid ' . s:initPid
    echo 's:wakeFile ' . s:wakeFile
    echo 's:tempFile ' . s:tempFile
    echo 's:friendProcess ' . s:friendProcess
    echo 's:overviewProcess ' . s:overviewProcess
    echo 's:colorFile ' . s:colorFile
    echo 's:errFile ' . s:errFile
    echo 's:windowId ' . s:windowId
endfunction "}}}

" Only way to get the correct windowid on unix, is to call gvim
" with the the --echo-wid, redirecting the output somewhere,
fun! s:DefineWindowId() "{{{
	let idFile = '/tmp/vimwinId'
    if has('win32') || has('mac') || !filereadable(idFile)
       let s:windowId = 0
       return
    endif

    for line in readfile(idFile)
    	if line =~ 'WID:'
            let s:windowId = substitute(line, '.*WID: \(\d\+\).*', '\1', '')
            return
        endif
    endfor

    let s:windowId = 0
endfunction "}}}

fun! s:ConvertColorSchemeToColorConf() "{{{
    let colorList = split(globpath( &rtp, 'colors/*.vim' ), '\n')

    for colorFilename in colorList
    	let colorName = substitute(colorFilename, '^.*[\\/]\([^\\\/]\+\)\.vim$', '\1', '')
    	echo "Dumping " . colorName
        highlight clear
    	exe 'color ' . colorName
        call s:BuildColorConfFromColorScheme(colorName . '.color')
    endfor

    highlight clear
    color default
    call s:BuildColorConfFromColorScheme('default.color')
endfunction "}}}

" If we want to use the same color as the colorscheme,
" we must prepare a configuration file with some infos.
fun! s:BuildColorConfFromColorScheme(filename) "{{{
	let conf =
        \ [ ["comment"     , 'comment'     , 'fg']
        \ , ["normal"      , 'normal'      , 'fg']
        \ , ["maj"         , 'normal'      , 'fg']
        \ , ["empty"       , 'normal'      , 'bg']
        \ , ["string"      , 'string'      , 'fg']
        \ , ["keyword"     , 'keyword'     , 'fg']
        \ , ["type"        , 'type'        , 'fg']
        \ , ["view"        , 'cursorline'  , 'bg']
        \ , ["typedef"     , 'Typedef'     , 'fg']
        \ , ["include"     , 'Include'     , 'fg']
        \
        \ , ["conditional" , 'Conditional' , 'fg']
        \ , ["repeat"      , 'Repeat'      , 'fg']
        \ , ["structure"   , 'Structure'   , 'fg']
        \ , ["statement"   , 'Statement'   , 'fg']
        \ , ["preproc"     , 'Preproc'     , 'fg']
        \ , ["exception"   , 'Exception'   , 'fg']
        \ , ["operator"    , 'Operator'    , 'fg']
        \ , ["storageClass", 'StorageClass', 'fg']
        \
        \ , ["float"       , 'Float'       , 'fg']
        \ , ["number"      , 'Number'      , 'fg']
        \ , ["bool"        , 'Boolean'     , 'fg']
        \ , ["char"        , 'Character'   , 'fg']
        \
        \ , ["label"       , 'Label'       , 'fg']
        \ , ["macro"       , 'Macro'       , 'fg']
        \
        \ , ["errorLine"   , 'Error'       , 'bg']
        \ , ["warningLine" , 'Todo'        , 'bg']
        \ , ["infoLine"    , 'IncSearch'   , 'bg']
        \
        \ ]
    
    let writtenConf = []

    for [progval, vimAttr, info] in conf
        let foundColor = synIDattr(synIDtrans(hlID(vimAttr)), info)
        if foundColor != ''
            call add( writtenConf, progval . '=' . foundColor )
        endif
    endfor

    call writefile(writtenConf, a:filename)
endfunction "}}}

fun! s:UpdateColorScheme() "{{{
    call s:BuildColorConfFromColorScheme(s:colorFile)
    call s:SnapshotFile()
endfunction "}}}

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
       let s:rmCommand = "rm -f "
       let s:tempCommandFile = s:tempDir . 'command.sh'
    endif

    let s:initPid = string(getpid())
    let s:wakeFile = s:tempDir . 'overviewFile' . s:initPid . '.txt'
    let s:tempFile = s:tempDir . 'previewer' . s:initPid . '.png'
    let s:colorFile = s:tempDir . 'colorFile' . s:initPid
    let s:errFile = s:tempDir . 'errFile' . s:initPid

    call s:DefineWindowId()

    execute 'set wildignore=' . s:tempWildIgnore

    let s:preparedParameters = 1
endfunction "}}}

fun! s:InitialInit() "{{{
    " Some version of vim don't get globpath with additional
    " flag to avoid wildignore, so we must do it by hand
    let s:tempWildIgnore = &wildignore
    set wildignore=

    if has('win32') || has('win64')
       let s:friendProcess = '"' . globpath( &rtp, 'plugin/WpfOverview.exe' ) . '"'
       let s:overviewProcess = '"' . globpath( &rtp, 'plugin/codeoverview.exe' ) . '"'
    elseif has('mac')
       let s:friendProcess = '"' . globpath( &rtp, 'plugin/CodeOverMac.app' ) . '"'
       let s:overviewProcess = '"' . globpath( &rtp, 'plugin/codeoverview.osx' ) . '"'
    else
       let s:friendProcess = '"' . globpath( &rtp, 'plugin/gtkOverview.py' ) . '"'
       let s:overviewProcess = '"' . globpath( &rtp, 'plugin/codeoverview' ) . '"'
    endif

    execute 'set wildignore=' . s:tempWildIgnore
endfunction "}}}

fun! s:RemoveTempsFile() "{{{
    call delete( s:tempFile )
    call delete( s:tempCommandFile )
    call delete( s:errFile )

    if g:code_overview_use_colorscheme
        call delete( s:colorFile )
    endif 
    call delete( s:wakeFile )
endfunction "}}}

fun! s:StopFriendProcess() "{{{
    if s:friendProcessStarted == 0
        echo 'Friend process is already stopped'
        return
    endif

    call writefile( ["quit"], s:wakeFile )
    let s:friendProcessStarted = 0

    command! CodeOverviewNoAuto echo 'CodeOverview Friend Process not started!'
    command! CodeOverviewAuto echo 'CodeOverview Friend Process not started!'
    command! SnapshotFile echo 'CodeOverview Friend Process not started!'

    call s:RemoveCodeOverviewHook()
    call s:RemoveTempsFile()
endfunction "}}}

" Launch the tracking window for this instance of gVIM
" Configure some script variables used in this script.
fun! s:LaunchFriendProcess() "{{{
    if s:friendProcessStarted == 1
        echo 'Friend process already started'
        return
    endif

    call s:PrepareParameters()

    if g:code_overview_use_colorscheme
        call s:BuildColorConfFromColorScheme(s:colorFile)
    endif

    " Just to be sure the file is created
    call writefile( [""], s:wakeFile )
    call s:SnapshotFile('')

    if has('win32')
        call system('cmd /s /c "start "CodeOverview Launcher" /b '
                \ . s:friendProcess . ' ' . s:initPid . '"')
    elseif has('mac')
        echo 'open -a ' . s:friendProcess . ' -p ' . s:initPid 
        call system('open -a ' . s:friendProcess . ' --args -p ' . s:initPid )
    else
        call system(s:friendProcess . ' ' . s:initPid . ' &')
    endif

    let s:friendProcessStarted = 1

    if exists("g:codeoverview_autoupdate")
        call s:PutCodeOverviewHook()
    endif

    command! CodeOverviewNoAuto call s:RemoveCodeOverviewHook()
    command! CodeOverviewAuto call s:PutCodeOverviewHook()
    command! SnapshotFile call s:SnapshotFile('')

    call s:SnapshotFile('')

endfunction "}}}

fun! s:SelectClass( kind, error ) "{{{
	if a:kind == 'search'
		return 'i'
    endif

    if a:error =~ '\cwarning'
        return 'w'
    else
        return 'e'
    endif
endfunction "}}}

" Kind could be 'e' for error, 'w' for
" warning 'i' for info...
fun! s:DumpErrorLines(kind) "{{{
	let outLines = []
	let currentBuffer = bufnr('%')

	for d in getqflist()
		if d.bufnr == currentBuffer
            call add(outLines, s:SelectClass(a:kind, d.type) . ':' . string(d.lnum))
        endif
    endfor

    call writefile(outLines, s:errFile)
endfunction "}}}

" This fuction extract data from the current view,
" generate an overview image of the current file,
" write an in an update file readen by the following
" window.
fun! s:SnapshotFile(kind) "{{{
    if line('$') > g:codeOverviewMaxLineCount
        echo 'File to big, no overview generated'
        return
    endif

    if a:kind != ''
        let s:lastQuickfixKind = a:kind
    endif

    if bufname('%') == '' ||
     \ index(g:code_overview_ignore_buffer_list,bufname('%')) >= 0
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
        let filename = '"' . expand( '%:p' ) . '"'
    endif

    let lastVisibleLine = line('w$')
    let winInfo = winsaveview()
    let research = getreg('/')

    " Generate the new image file
    let commandLine = s:overviewProcess . ' -v -o "' . s:tempFile . '" ' 

    " If we search an identifier
    if research =~ '\\<.*\\>'
        " Add a switch to let the image generator make it.
        let commandLine = commandLine . ' --hi ' . substitute( research, '\\<\(.*\)\\>', '\1', '' ) . ' '
    endif

    " Dump error lines
    if g:codeOverviewShowErrorLines
    	call s:DumpErrorLines(s:lastQuickfixKind)
    	let commandLine  = commandLine . ' --errfile ' . s:errFile
    endif

    if g:code_overview_use_colorscheme
        let commandLine = commandLine . ' --conf ' . s:colorFile . ' '
    endif

    let commandLine = commandLine . " " . filename

    if !has('win32')
        let header = '#!/bin/sh'
    else
    	let header = ''
    endif

    let wakeText = string(winInfo.topline) 
               \ . '?' . string(lastVisibleLine)
               \ . '?' . synIDattr(synIDtrans(hlID('Normal')), 'bg')
               \ . '?' . synIDattr(synIDtrans(hlID('CursorLine')), 'bg')
               \ . '?' . s:windowId
               \ . '?' . string(getwinposx())
               \ . '?' . string(getwinposy())
               \ . '?' . s:tempFile

    " Make an non-blocking start
    if has('win32')
        let wakeCommand = 'echo ' . wakeText . ' > "' . s:wakeFile . '"'
        let localeSwitch = ''
    else
        let wakeCommand = 'echo "' . wakeText . '" > "' . s:wakeFile . '"'
        " Problem arise with mismatch of locale, so set it to a working
        " one
        let localSwitch = 'LC_ALL=en_US.utf8'
    endif

    let commandFile = [ header
                    \ , s:rmCommand . '"' . s:tempFile . '"'
                    \ , localSwitch
                    \ , commandLine
                    \ , wakeCommand
                    \ ]

    if &modified
    	call add(commandFile, s:rmCommand . filename )
    endif

    call writefile( commandFile, s:tempCommandFile )

    if has('win32')
        call system( '"' . s:tempCommandFile . '"' )
    else 
    	call system( 'sh "' . s:tempCommandFile . '" &' )
    endif
endfunction "}}}

fun! s:PutCodeOverviewHook() "{{{
    augroup CodeOverview
        au BufNewFile * call s:SnapshotFile('')
        au BufEnter * call s:SnapshotFile('')
        au BufNew * call s:SnapshotFile('')
        au BufWritePost * call s:SnapshotFile('')
        au FilterWritePost * call s:SnapshotFile('')
        au StdinReadPost * call s:SnapshotFile('')
        au FileChangedShellPost * call s:SnapshotFile('')
        au QuickFixCmdPost *grep* call s:SnapshotFile('search')
        au QuickFixCmdPost *make call s:SnapshotFile('build')
    augroup END
endfunction "}}}

fun! s:RemoveCodeOverviewHook() "{{{
    augroup CodeOverview
        au!
    augroup END
endfunction "}}}

call s:InitialInit()

command! DumpAllColorSchemes call s:ConvertColorSchemeToColorConf()

if s:friendProcess == '""' || s:overviewProcess == '""'
    echo "Can't find friend executables, aborting CodeOverview load"
    finish
endif

au VimLeavePre * call s:StopFriendProcess()
au ColorScheme * call s:UpdateColorScheme()

command! CodeOverviewNoAuto echo 'CodeOverview Friend Process not started!'
command! CodeOverviewAuto echo 'CodeOverview Friend Process not started!'
command! SnapshotFile echo 'CodeOverview Friend Process not started!'
command! ShowCodeOverview call s:LaunchFriendProcess()
command! HideCodeOverview call s:StopFriendProcess()

if exists("g:code_overview_autostart")
	call s:LaunchFriendProcess()
endif

