
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-01-two-files example

Materalize all targets:

  $ ./jenga.exe build --local-cache -m
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

  $ ./jenga.exe build --local-cache
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/main.exe
