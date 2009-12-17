
BUILD instruction
=================

for the GUI :
    - Start the .sln
    - Compile WpfOverview
    - Done

for the PNG generator (you need haskell for this one) :
    - make conf
    - make
    - Done
    - Optional additional step to reduce exe size :
        - strip codeoverview.exe
        - upx codeoverview.exe

Normally you should have 2 exe in the current folder, otherwise, just
dig dist/build/codeoverview/codeoverview.exe to get the png generator

