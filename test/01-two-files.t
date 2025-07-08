
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

  $ ./jenga.exe build --cache=.
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc -c fib.c -o fib.o
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
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

  $ ./jenga.exe build -c.
  elaborated 3 rules and 3 targets
  materalizing 1 artifact

Add '-x' flag for a more detailed logging.

  $ ./jenga.exe build -c. -x
  X: md5sum example/build.jenga
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  X: md5sum example/fib.c
  X: md5sum example/main.c

Add '-xi flags for very detailed logging. Test will be fragile to any change in example source.
We take care to mask any pids referenced by sanboxes

  $ ./jenga.exe build -c. -xi | sed 's|jbox/[0-9]*|jbox/$$|'
  I: rm -rf ,jenga
  I: mkdir -p .cache/jenga/files
  I: mkdir -p .cache/jenga/traces
  I: mkdir -p ,jenga
  I: test -e .
  I: test -d .
  I: ls .
  I: test -d jenga.exe
  I: test -d .cache
  I: test -d example
  I: test -d ,jenga
  I: ls example
  I: test -d example/fib.c
  I: test -d example/main.c
  I: test -d example/build.jenga
  I: ls .
  I: ls example
  I: ls example
  I: test -e example/all.files
  I: test -e example/build.jenga
  X: md5sum example/build.jenga
  I: test -e .cache/jenga/files/88547b1673021d538aa20da95fc4b55c
  I: cat .cache/jenga/files/88547b1673021d538aa20da95fc4b55c
  I: test -e example/main.o
  I: test -e example/fib.o
  I: test -e example/main.exe
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  I: test -e example/fib.c
  X: md5sum example/fib.c
  I: test -e .cache/jenga/files/3ec221831446382d711ea3ce24237158
  I: test -e .cache/jenga/traces/f58b40a6cdffe882c9832ef266d9fab6
  I: cat .cache/jenga/traces/f58b40a6cdffe882c9832ef266d9fab6
  I: test -e .cache/jenga/files/47a0ee09b975f7501dbeb5431b76c24c
  I: test -e example/main.c
  X: md5sum example/main.c
  I: test -e .cache/jenga/files/9d125f57501617a7e09da68a33e65d1c
  I: test -e .cache/jenga/traces/17d39e69d68222323ce53e36a1fd0dba
  I: cat .cache/jenga/traces/17d39e69d68222323ce53e36a1fd0dba
  I: test -e .cache/jenga/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: test -e .cache/jenga/traces/77ee6c82e36af75984a27ac518621689
  I: cat .cache/jenga/traces/77ee6c82e36af75984a27ac518621689
  I: test -e .cache/jenga/files/9efc05831ccef0c24b2697d8fff2acee
  I: mkdir -p ,jenga/example
  I: ln .cache/jenga/files/9efc05831ccef0c24b2697d8fff2acee ,jenga/example/main.exe
  I: rm -rf /tmp/.jbox/$$

Update main.c "world->UNIVERSE" and rerun:

  $ sed -i 's/world/UNIVERSE/g' example/main.c
  $ ./jenga.exe build -c.
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/example/main.exe
  hello, 55 UNIVERSE

Reverting to previous state of main.c causes no rebuilding:

  $ sed -i 's/UNIVERSE/world/g' example/main.c
  $ ./jenga.exe build -c.
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  $ ,jenga/example/main.exe
  hello, 55 world

Whitespace only change to main.c cause no link step (early cutoff):

  $ sed -i 's/int main/int      main/g' example/main.c
  $ ./jenga.exe build -c.
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc -c main.c -o main.o
  ran 1 action

Update build rules to link executable under a different name:

  $ sed -i 's/main.exe/RENAMED.exe/' example/build.jenga
  $ ./jenga.exe build -c.
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc fib.o main.o -o RENAMED.exe
  ran 1 action

  $ ,jenga/example/RENAMED.exe
  hello, 55 world

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/RENAMED.exe

Relocate the example to a new directory; no rebuilds:

  $ mv example RELOCATED
  $ ./jenga.exe build -c.
  elaborated 3 rules and 3 targets
  materalizing 1 artifact

  $ find ,jenga
  ,jenga
  ,jenga/RELOCATED
  ,jenga/RELOCATED/RENAMED.exe

Duplicate the example directory; double elaborated rules; still no rebuilds:

  $ cp -rp RELOCATED ANOTHER
  $ ./jenga.exe build -c.
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
  $ ./jenga.exe build -c.
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o RENAMED.exe
  ran 2 actions

Run the two versions:

  $ ,jenga/RELOCATED/RENAMED.exe
  hello, 6765 world
  $ ,jenga/ANOTHER/RENAMED.exe
  hello, 55 world

Materalize all targets:

  $ ./jenga.exe build -c. -m
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

  $ ./jenga.exe build -c.
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts

  $ find ,jenga
  ,jenga
  ,jenga/ANOTHER
  ,jenga/ANOTHER/RENAMED.exe
  ,jenga/RELOCATED
  ,jenga/RELOCATED/RENAMED.exe
