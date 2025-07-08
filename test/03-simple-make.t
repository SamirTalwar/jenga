
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/03-simple-make example

  $ jenga build
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

