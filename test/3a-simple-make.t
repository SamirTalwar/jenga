
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example3-simple-make example

  $ find .
  .
  ./jenga.exe
  ./example
  ./example/defs.h.in
  ./example/fib.c
  ./example/main.c
  ./example/fib.h
  ./example/README
  ./example/simple-make.jenga

  $ ./jenga.exe build -a
  elaborated 4 rules and 4 targets
  materalizing 4 artifacts
  A: cd ,jenga/box/0; grep -v '^$' defs.h.in > defs.h
  A: cd ,jenga/box/1; gcc -c fib.c -o fib.o
  A: cd ,jenga/box/2; gcc -c main.c -o main.o
  A: cd ,jenga/box/3; gcc fib.o main.o -o main.exe
  ran 4 actions

  $ find ,jenga
  ,jenga
  ,jenga/artifacts
  ,jenga/artifacts/example
  ,jenga/artifacts/example/fib.o
  ,jenga/artifacts/example/main.exe
  ,jenga/artifacts/example/defs.h
  ,jenga/artifacts/example/main.o
  ,jenga/box

  $ ,jenga/artifacts/example/main.exe
  hello, 55 world with explicit make-style rules

