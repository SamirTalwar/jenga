
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-03-simple-make example

  $ find .
  .
  ./jenga.exe
  ./example
  ./example/defs.h.in
  ./example/fib.c
  ./example/main.c
  ./example/fib.h
  ./example/README
  ./example/build.jenga

  $ ./jenga.exe build --local-cache -a
  elaborated 4 rules and 4 targets
  materalizing 1 artifact
  A: gcc -c fib.c -o fib.o
  A: grep -v '^$' defs.h.in > defs.h
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 4 actions

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/main.exe

  $ ,jenga/example/main.exe
  hello, 55 world with explicit make-style rules

