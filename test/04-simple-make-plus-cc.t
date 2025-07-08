
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/04-simple-make-plus-cc example

  $ jenga build
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

