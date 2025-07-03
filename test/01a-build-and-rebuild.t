
Get me a jenga executable and the source code for the first example...

  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-01-two-files example

What have I got?

  $ find .
  .
  ./jenga.exe
  ./example
  ./example/fib.c
  ./example/main.c
  ./example/build.jenga

Build from clean:

  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd .jbox/0; gcc -c fib.c -o fib.o
  A: cd .jbox/1; gcc -c main.c -o main.o
  A: cd .jbox/2; gcc fib.o main.o -o main.exe
  ran 3 actions

Rebuild after no changes:

  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
