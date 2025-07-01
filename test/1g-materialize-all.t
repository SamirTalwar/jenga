
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example1 example

Materalize all targets:

  $ ./jenga.exe build -m
  elaborated 3 rules and 3 targets
  materalizing all targets
  ran 3 actions

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/main.exe
  ,jenga/example/main.o

Materalize just artifacts:

  $ ./jenga.exe build
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/main.exe
