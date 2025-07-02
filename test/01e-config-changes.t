
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-01-two-files example

Build from clean and run:

  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd .jbox/0; gcc -c fib.c -o fib.o
  A: cd .jbox/1; gcc -c main.c -o main.o
  A: cd .jbox/2; gcc fib.o main.o -o main.exe
  ran 3 actions
  $ ,jenga/example/main.exe
  hello, 55 world

Update build rules to link executable under a different name:

  $ sed -i 's/main.exe/RENAMED.exe/' example/make.jenga
  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd .jbox/0; gcc fib.o main.o -o RENAMED.exe
  ran 1 action

Check we cannot find the executable by its old name:
  $ ,jenga/example/main.exe
  /bin/sh: 14: ,jenga/example/main.exe: not found
  [127]

But only via its new name:
  $ ,jenga/example/RENAMED.exe
  hello, 55 world

See the final artifacts:

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/RENAMED.exe
