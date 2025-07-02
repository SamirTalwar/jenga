
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example6-diamond example

  $ ./jenga.exe build --local-cache -a
  elaborated 6 rules and 6 targets
  materalizing 1 artifact
  A: cd .jbox/0; echo -n A > a
  A: cd .jbox/1; echo -n B  > b
  A: cd .jbox/2; cat a b > ab
  A: cd .jbox/3; echo -n C > c
  A: cd .jbox/4; cat b c > bc
  A: cd .jbox/5; cat ab bc > top
  ran 6 actions

  $ cat ,jenga/example/top
  ABBC (no-eol)

The example has a diamond dependency on target 'b', reached via 'ab' and 'bc'.
During any build (incuding a zero-rebuild) we should require 'b' more than once.

  $ ./jenga.exe build --local-cache -b | grep Require
  B: Require: example/make.jenga
  B: Require: example/top
  B: Require: example/ab
  B: Require: example/a
  B: Require: example/b
  B: Require: example/bc
  B: Require: example/c
