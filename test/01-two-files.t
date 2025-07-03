
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

See the artifacts:

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/main.exe

Run the built artifact:

  $ ,jenga/example/main.exe
  hello, 55 world

Rebuild after no changes:

  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact

Add '-x' flag for a more detailed logging.

  $ ./jenga.exe build --local-cache -ax
  X: md5sum example/build.jenga
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  X: md5sum example/fib.c
  X: md5sum example/main.c

Add '-xi flags for very detailed logging. Test will be fragile to any change in example source.

  $ ./jenga.exe build --local-cache -axi
  I: rm -rf .jbox
  I: rm -rf ,jenga
  I: mkdir -p .cache/files
  I: mkdir -p .cache/traces
  I: mkdir -p .jbox
  I: mkdir -p ,jenga
  I: test -e .
  I: test -d .
  I: ls .
  I: test -d jenga.exe
  I: test -d .cache
  I: test -d example
  I: test -d ,jenga
  I: test -d .jbox
  I: ls example
  I: test -d example/fib.c
  I: test -d example/main.c
  I: test -d example/build.jenga
  I: ls .jbox
  I: ls .
  I: ls .jbox
  I: ls example
  X: md5sum example/build.jenga
  I: test -e .cache/files/ade02a5a8e9b16d13646fc43aa2e61ca
  I: cat example/build.jenga
  I: test -e example/main.o
  I: test -e example/fib.o
  I: test -e example/main.exe
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  X: md5sum example/fib.c
  I: test -e .cache/files/3ec221831446382d711ea3ce24237158
  I: md5sum
  I: test -e .cache/traces/347c90127bbc1461aecd80702c3c5bc4
  I: cat .cache/traces/347c90127bbc1461aecd80702c3c5bc4
  I: test -e .cache/files/47a0ee09b975f7501dbeb5431b76c24c
  X: md5sum example/main.c
  I: test -e .cache/files/9d125f57501617a7e09da68a33e65d1c
  I: md5sum
  I: test -e .cache/traces/6ce82221b5bd8437a672f4e6d19a9d9e
  I: cat .cache/traces/6ce82221b5bd8437a672f4e6d19a9d9e
  I: test -e .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: md5sum
  I: test -e .cache/traces/4677478c51f6bbba384ed576c9e451ed
  I: cat .cache/traces/4677478c51f6bbba384ed576c9e451ed
  I: test -e .cache/files/9efc05831ccef0c24b2697d8fff2acee
  I: mkdir -p ,jenga/example
  I: ln .cache/files/9efc05831ccef0c24b2697d8fff2acee ,jenga/example/main.exe

Update main.c "world->UNIVERSE" and rerun:

  $ sed -i 's/world/UNIVERSE/g' example/main.c
  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd .jbox/0; gcc -c main.c -o main.o
  A: cd .jbox/1; gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/example/main.exe
  hello, 55 UNIVERSE

Reverting to previous state of main.c causes no rebuilding:

  $ sed -i 's/UNIVERSE/world/g' example/main.c
  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  $ ,jenga/example/main.exe
  hello, 55 world

Whitespace only change to main.c cause no link step (early cutoff):

  $ sed -i 's/int main/int      main/g' example/main.c
  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd .jbox/0; gcc -c main.c -o main.o
  ran 1 action

Update build rules to link executable under a different name:

  $ sed -i 's/main.exe/RENAMED.exe/' example/build.jenga
  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd .jbox/0; gcc fib.o main.o -o RENAMED.exe
  ran 1 action

  $ ,jenga/example/RENAMED.exe
  hello, 55 world

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/RENAMED.exe

Relocate the example to a new directory; no rebuilds:

  $ mv example RELOCATED
  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact

  $ find ,jenga
  ,jenga
  ,jenga/RELOCATED
  ,jenga/RELOCATED/RENAMED.exe

Duplicate the example directory; double elaborated rules; still no rebuilds:

  $ cp -rp RELOCATED ANOTHER
  $ ./jenga.exe build --local-cache -a
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts

  $ find ,jenga
  ,jenga
  ,jenga/ANOTHER
  ,jenga/ANOTHER/RENAMED.exe
  ,jenga/RELOCATED
  ,jenga/RELOCATED/RENAMED.exe

Modify code in one of the example directories; minimal rebuild as required:

  $ sed -i 's/fib(10)/fib(20)/g' RELOCATED/main.c
  $ ./jenga.exe build --local-cache -a
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts
  A: cd .jbox/0; gcc -c main.c -o main.o
  A: cd .jbox/1; gcc fib.o main.o -o RENAMED.exe
  ran 2 actions

Run the two versions:

  $ ,jenga/RELOCATED/RENAMED.exe
  hello, 6765 world
  $ ,jenga/ANOTHER/RENAMED.exe
  hello, 55 world

Materalize all targets:

  $ ./jenga.exe build --local-cache -m
  elaborated 6 rules and 6 targets
  materalizing all targets

  $ find ,jenga
  ,jenga
  ,jenga/ANOTHER
  ,jenga/ANOTHER/fib.o
  ,jenga/ANOTHER/RENAMED.exe
  ,jenga/ANOTHER/main.o
  ,jenga/RELOCATED
  ,jenga/RELOCATED/fib.o
  ,jenga/RELOCATED/RENAMED.exe
  ,jenga/RELOCATED/main.o

Materalize just artifacts:

  $ ./jenga.exe build --local-cache
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts

  $ find ,jenga
  ,jenga
  ,jenga/ANOTHER
  ,jenga/ANOTHER/RENAMED.exe
  ,jenga/RELOCATED
  ,jenga/RELOCATED/RENAMED.exe

Blow away the local cache and rebuild keeping sandboxes:

  $ rm -rf .cache
  $ ./jenga.exe build --local-cache -k
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts
  ran 5 actions

  $ find .jbox
  .jbox
  .jbox/0
  .jbox/0/fib.o
  .jbox/0/fib.c
  .jbox/4
  .jbox/4/fib.o
  .jbox/4/RENAMED.exe
  .jbox/4/main.o
  .jbox/3
  .jbox/3/main.c
  .jbox/3/main.o
  .jbox/1
  .jbox/1/main.c
  .jbox/1/main.o
  .jbox/2
  .jbox/2/fib.o
  .jbox/2/RENAMED.exe
  .jbox/2/main.o

Zero rebuild and the sandboxes are removed

  $ ./jenga.exe build --local-cache
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts

  $ find .jbox
  .jbox
