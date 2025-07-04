
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-04-simple-make-plus-cc example

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

  $ ./jenga.exe build -c. -a
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts
  A: gcc -MG -MM fib.c -MF fib.d
  A: gcc -c fib.c -o fib.o
  A: gcc -MG -MM main.c -MF main.d
  A: grep -v '^$' defs.h.in > defs.h
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 6 actions

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/main.exe
  ,jenga/example/defs.h

  $ ,jenga/example/main.exe
  hello, 55 world with combined cc and make configs

