ifeq ($(shell uname),WindowsNT)
SHELL:=cmd
EXEEXT:=.exe
HOMEDIR:=${USERPROFILE}
else
HOMEDIR:=${HOME}
ifeq ($(shell uname),Darwin)
EXEEXT:=.osx
else
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

