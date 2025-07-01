
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example1 example

Build from clean and run:

  $ ./jenga.exe build -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd .jbox/0; gcc -c fib.c -o fib.o
  A: cd .jbox/1; gcc -c main.c -o main.o
  A: cd .jbox/2; gcc fib.o main.o -o main.exe
  ran 3 actions
  $ ,jenga/example/main.exe
  hello, 55 world

Update config file to link executable with a different name:

  $ echo RENAMED.exe > example/cc-basic.jenga
  $ ./jenga.exe build -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd .jbox/0; gcc fib.o main.o -o RENAMED.exe
  ran 1 action
  $ ,jenga/example/main.exe
  /bin/sh: 14: ,jenga/example/main.exe: not found
  [127]
  $ ,jenga/example/RENAMED.exe
  hello, 55 world

See the final artifacts:

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/RENAMED.exe
