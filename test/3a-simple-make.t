
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

  $ ./jenga.exe build --local-cache -a
  elaborated 4 rules and 4 targets
  materalizing 4 artifacts
  A: cd .jbox/0; grep -v '^$' defs.h.in > defs.h
  A: cd .jbox/1; gcc -c fib.c -o fib.o
  A: cd .jbox/2; gcc -c main.c -o main.o
  A: cd .jbox/3; gcc fib.o main.o -o main.exe
  ran 4 actions

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/main.exe
  ,jenga/example/defs.h
  ,jenga/example/main.o

  $ ,jenga/example/main.exe
  hello, 55 world with explicit make-style rules

