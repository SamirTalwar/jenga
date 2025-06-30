
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example4-simple-make-plus-cc example

  $ find .
  .
  ./jenga.exe
  ./example
  ./example/defs.h.in
  ./example/fib.c
  ./example/main.c
  ./example/fib.h
  ./example/cc.jenga
  ./example/README
  ./example/simple-make.jenga

  $ ./jenga.exe build -a
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts
  A: cd ,jenga/box/0; grep -v '^$' defs.h.in > defs.h
  A: cd ,jenga/box/1; gcc -MG -MM fib.c -MF fib.d
  A: cd ,jenga/box/2; gcc -c fib.c -o fib.o
  A: cd ,jenga/box/3; gcc -MG -MM main.c -MF main.d
  A: cd ,jenga/box/4; gcc -c main.c -o main.o
  A: cd ,jenga/box/5; gcc fib.o main.o -o main.exe
  ran 6 actions

  $ find ,jenga
  ,jenga
  ,jenga/artifacts
  ,jenga/artifacts/example
  ,jenga/artifacts/example/main.exe
  ,jenga/artifacts/example/defs.h
  ,jenga/box

  $ ,jenga/artifacts/example/main.exe
  hello, 55 world with combined cc and make configs

