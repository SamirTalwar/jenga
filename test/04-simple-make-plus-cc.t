
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
  ./example/make.jenga

  $ ./jenga.exe build --local-cache -a
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts
  A: cd .jbox/0; gcc -MG -MM fib.c -MF fib.d
  A: cd .jbox/1; gcc -c fib.c -o fib.o
  A: cd .jbox/2; gcc -MG -MM main.c -MF main.d
  A: cd .jbox/3; grep -v '^$' defs.h.in > defs.h
  A: cd .jbox/4; gcc -c main.c -o main.o
  A: cd .jbox/5; gcc fib.o main.o -o main.exe
  ran 6 actions

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/main.exe
  ,jenga/example/defs.h

  $ ,jenga/example/main.exe
  hello, 55 world with combined cc and make configs

