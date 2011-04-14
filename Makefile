build:
	#runhaskell Setup.hs build
	ghc --make -O2 -Wall -o ~/vimfiles/bundle/CodeOverview/plugin/codeoverview codeOverviewMain.hs
	#cp codeoverview ~/vimfiles/bundle/CodeOverview/plugin/
	#cp dist/build/codeoverview/codeoverview.exe .

run:
	codeoverview.exe png.hs
	codeoverview.exe codeoverview.hs

conf:
	runhaskell Setup.hs configure

clean:
	runhaskell Setup.hs clean

