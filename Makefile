build:
	runhaskell Setup.hs build
	cp dist/build/codeoverview/codeoverview.exe .

run:
	codeoverview.exe png.hs
	codeoverview.exe codeoverview.hs

conf:
	runhaskell Setup.hs configure

clean:
	runhaskell Setup.hs clean

