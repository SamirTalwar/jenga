
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example1 example

Add '-x' flag for a more detailed logging.

Build from clean:

  $ ./jenga.exe build example -a -x
  X: md5sum example/cc-basic.jc
  elaborated 3 rules and 1 root
  X: md5sum example/fib.c
  A: cd ,jenga/box/0; gcc -c fib.c -o fib.o
  X: md5sum ,jenga/box/0/fib.o
  X: md5sum example/main.c
  A: cd ,jenga/box/1; gcc -c main.c -o main.o
  X: md5sum ,jenga/box/1/main.o
  A: cd ,jenga/box/2; gcc fib.o main.o -o main.exe
  X: md5sum ,jenga/box/2/main.exe
  ran 3 actions

Rebuild after no changes:

  $ ./jenga.exe build example -a -x
  X: md5sum example/cc-basic.jc
  elaborated 3 rules and 1 root
  X: md5sum example/fib.c
  X: md5sum example/main.c
