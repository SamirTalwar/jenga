
Get me a jenga executable and the source code for the first example...

  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example1 example

What have I got?

  $ find .
  .
  ./jenga.exe
  ./example
  ./example/fib.c
  ./example/main.c
  ./example/fib.h
  ./example/config

Build from clean:

  $ ./jenga.exe build example -a
  elaborated 3 rules and 1 root
  A: cd ,jenga/box/0; gcc -c fib.c -o fib.o
  A: cd ,jenga/box/1; gcc -c main.c -o main.o
  A: cd ,jenga/box/2; gcc fib.o main.o -o main.exe
  ran 3 actions

Rebuild after no changes:

  $ ./jenga.exe build example -a
  elaborated 3 rules and 1 root
