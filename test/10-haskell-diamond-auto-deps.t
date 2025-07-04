
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-10-haskell-diamond-auto-deps example

  $ ./jenga.exe build -c.
  elaborated 10 rules and 15 targets
  materalizing 6 artifacts
  A: find $HOME/.stack | grep -v lib | grep bin/ghc$ | sort -n | tail -1 > ghc-path
  A: echo exec $(cat ghc-path) '"$@"' > ghc.exe ; chmod +x ghc.exe
  A: echo 'import Top' > main.hs
  A: ./ghc.exe -M *.hs -dep-makefile depends
  A: ./ghc.exe -c A.hs
  A: ./ghc.exe -c C.hs
  A: ./ghc.exe -c B.hs
  A: ./ghc.exe -c Top.hs
  A: ./ghc.exe -c main.hs
  A: ./ghc.exe -o diamond.exe main.o Top.o B.o C.o A.o
  ran 10 actions

  $ ,jenga/example/diamond.exe
  Top[B[A],C[A]]

  $ ./jenga.exe build -c. -v
  elaborated 10 rules and 15 targets
  materalizing 6 artifacts
  B: Require: example/main.hi
  B: Require: example/depends
  B: Require: example/ghc.exe
  B: Require: example/ghc-path
  B: Require: example/main.hs
  B: Require: example/Top.hi
  B: Require: example/C.hi
  B: Require: example/A.hi
  B: Require: example/B.hi
  B: Require: example/diamond.exe
  B: Require: example/main.o
  B: Require: example/Top.o
  B: Require: example/B.o
  B: Require: example/C.o
  B: Require: example/A.o
