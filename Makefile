build:
	#runhaskell Setup.hs build
	ghc --make -O2 -Wall -o ~/vimfiles/bundle/CodeOverview/plugin/codeoverview.osx codeOverviewMain.hs
	#cp codeoverview ~/vimfiles/bundle/CodeOverview/plugin/
	#cp dist/build/codeoverview/codeoverview.exe .

run:
	LC_ALL=en_US.utf8
	/Users/vince/.vim/bundle/CodeOverview/plugin/codeoverview.osx -v -o meh.png --conf /tmp/colorFile20092  "/Users/vince/Documents/Coding/Tritonstable/Triton/EventManager.cpp"
	open meh.png
	#codeoverview.exe codeoverview.hs

conf:
	runhaskell Setup.hs configure

clean:
	runhaskell Setup.hs clean

