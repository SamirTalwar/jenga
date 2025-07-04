
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-05-sudoku example

  $ ./jenga.exe build --local-cache -a
  elaborated 7 rules and 8 targets
  materalizing 1 artifact
  A: find $HOME/.stack | grep lib/.*/bin/ghc$ | rev | cut -d/ -f3 | rev | cut -d- -f2 | sort -n | tail -1 > latest-version
  A: cp latest-version version
  A: find $HOME/.stack | grep lib/.*/bin/ghc$ | grep $(cat version) > ghc-path
  A: echo exec $(cat ghc-path) '"$@"' > ghc.exe; chmod +x ghc.exe
  A: ./ghc.exe -c Sudoku.hs -XLambdaCase
  A: ./ghc.exe -c main.hs
  A: ./ghc.exe main.o Sudoku.o -package containers -o solver.exe
  ran 7 actions

  $ ,jenga/example/solver.exe example/puzzle
  ...3.9..5
  ...475...
  ......4..
  89.......
  .7..5..89
  56....3.4
  ..7..492.
  ....9.6.8
  2...31.4.
  
  428369715
  913475862
  756812493
  892743156
  374156289
  561928374
  637584921
  145297638
  289631547
  
  1

