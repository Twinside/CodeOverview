ifeq ($(shell uname),WindowsNT)
SHELL:=cmd
EXEEXT:=.exe
HOMEDIR:=${USERPROFILE}
VIMDIR:=${HOMEDIR}/vimfiles
else
HOMEDIR:=${HOME}
VIMDIR:=${HOMEDIR}/.vim
ifeq ($(shell uname),Darwin)
HOMEDIR:=${HOME}
EXEEXT:=.osx
else
HOMEDIR:=${HOME}
EXEEXT:=
endif
endif

build:
	ghc --make -O2 -Wall -o ${HOMEDIR}/dotfiles/vimfiles/bundle/vim-codeoverview/plugin/codeoverview${EXEEXT} codeOverviewMain.hs

run:
	LC_ALL=en_US.utf8
	#codeoverview.exe codeoverview.hs

conf:
	runhaskell Setup.hs configure

clean:
	runhaskell Setup.hs clean

deploy:
	cp CodeOverview/plugin/CodeOverview.vim ${VIMDIR}/bundle/vim-codeoverview/plugin/CodeOverview.vim
	cp CodeOverview/syntax/codeoverview.vim ${VIMDIR}/bundle/vim-codeoverview/syntax/codeoverview.vim
	cp CodeOverview/ftdetect/codeoverview.vim ${VIMDIR}/bundle/vim-codeoverview/ftdetect/codeoverview.vim
