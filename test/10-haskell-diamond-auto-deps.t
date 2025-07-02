
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example10-haskell-diamond-auto-deps example

  $ ./jenga.exe build --local-cache -a
  elaborated 10 rules and 15 targets
  materalizing 6 artifacts
  A: cd .jbox/0; find $HOME/.stack | grep -v lib | grep bin/ghc$ | sort -n | tail -1 > ghc-path
  A: cd .jbox/1; echo exec $(cat ghc-path) '"$@"' > ghc.exe ; chmod +x ghc.exe
  A: cd .jbox/2; echo 'import Top' > main.hs
  A: cd .jbox/3; ./ghc.exe -M *.hs -dep-makefile depends
  A: cd .jbox/4; ./ghc.exe -c A.hs
  A: cd .jbox/5; ./ghc.exe -c C.hs
  A: cd .jbox/6; ./ghc.exe -c B.hs
  A: cd .jbox/7; ./ghc.exe -c Top.hs
  A: cd .jbox/8; ./ghc.exe -c main.hs
  A: cd .jbox/9; ./ghc.exe -o diamond.exe main.o Top.o B.o C.o A.o
  ran 10 actions

  $ ,jenga/example/diamond.exe
  Top[B[A],C[A]]

  $ ./jenga.exe build --local-cache -b | grep Require
  B: Require: example/make.jenga
  B: Require: example/main.hi
  B: Require: example/depends
  B: Require: example/ghc.exe
  B: Require: example/ghc-path
  B: Require: example/main.hs
  B: Require: example/A.hs
  B: Require: example/B.hs
  B: Require: example/C.hs
  B: Require: example/Top.hs
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
