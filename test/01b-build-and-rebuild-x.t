
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-01-two-files example

Add '-x' flag for a more detailed logging.

Build from clean:

  $ ./jenga.exe build --local-cache -ax
  X: md5sum example/build.jenga
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  X: md5sum example/fib.c
  A: cd .jbox/0; gcc -c fib.c -o fib.o
  X: md5sum .jbox/0/fib.o
  X: md5sum example/main.c
  A: cd .jbox/1; gcc -c main.c -o main.o
  X: md5sum .jbox/1/main.o
  A: cd .jbox/2; gcc fib.o main.o -o main.exe
  X: md5sum .jbox/2/main.exe
  ran 3 actions

Rebuild after no changes:

  $ ./jenga.exe build --local-cache -ax
  X: md5sum example/build.jenga
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  X: md5sum example/fib.c
  X: md5sum example/main.c
