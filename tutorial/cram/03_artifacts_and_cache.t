
This cram file accompanies the jenga tutorial.

Get an up-to-date jenga executable in path, which runs with a local cachee

  $ (cd $TESTDIR/../..; jenga build -q)
  $ echo exec $TESTDIR/../../,jenga/src/jenga '"$@"' > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get the example.

  $ cp -rp $TESTDIR/../files/03 example

Initial build. Expect 3 actions to be run

  $ jenga build -c.
  elaborated 3 rules and 3 targets
  A: gcc -Wall -c -o fib.o fib.c
  A: gcc -Wall -c -o main.o main.c
  A: gcc -o hello.exe main.o fib.o
  ran 3 actions

Zero build

  $ jenga build -c.
  elaborated 3 rules and 3 targets

Double build

  $ cp -rp example copied
  $ jenga build -c.
  elaborated 6 rules and 6 targets

What are the targets?

  $ jenga list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe
  copied/fib.o
  copied/main.o
  copied/hello.exe

  $ jenga list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe
  copied/fib.o
  copied/main.o
  copied/hello.exe

Controlling the scope of what to build

  $ jenga build copied -c.
  elaborated 3 rules and 3 targets

  $ jenga list-targets copied -c.
  copied/fib.o
  copied/main.o
  copied/hello.exe

Builds are relative
  $ cd copied
  $ jenga build -c..
  elaborated 3 rules and 3 targets
  $ cd ..

Using a non-default cache:

  $ jenga build --cache=my-cache
  elaborated 6 rules and 6 targets
  A: gcc -Wall -c -o fib.o fib.c
  A: gcc -Wall -c -o main.o main.c
  A: gcc -o hello.exe main.o fib.o
  ran 3 actions

Using a non-default cache (still get minimal builds)

  $ jenga build --cache=my-cache
  elaborated 6 rules and 6 targets

Using a temporary cache with -f. Forces run of all the actions

  $ jenga build -f | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
  using temporary cache: /tmp/.cache/jenga/$$
  elaborated 6 rules and 6 targets
  A: gcc -Wall -c -o fib.o fib.c
  A: gcc -Wall -c -o main.o main.c
  A: gcc -o hello.exe main.o fib.o
  ran 3 actions

  $ jenga build -f | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
  using temporary cache: /tmp/.cache/jenga/$$
  elaborated 6 rules and 6 targets
  A: gcc -Wall -c -o fib.o fib.c
  A: gcc -Wall -c -o main.o main.c
  A: gcc -o hello.exe main.o fib.o
  ran 3 actions

Where are the targets? ,jenga dir is created relative to where the build started

  $ jenga build
  elaborated 6 rules and 6 targets
  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/hello.exe
  ,jenga/example/main.o
  ,jenga/copied
  ,jenga/copied/fib.o
  ,jenga/copied/hello.exe
  ,jenga/copied/main.o

  $ jenga build copied
  elaborated 3 rules and 3 targets
  $ find ,jenga
  ,jenga
  ,jenga/copied
  ,jenga/copied/fib.o
  ,jenga/copied/hello.exe
  ,jenga/copied/main.o

  $ (cd copied && jenga build)
  elaborated 3 rules and 3 targets
  $ find copied/,jenga
  copied/,jenga
  copied/,jenga/fib.o
  copied/,jenga/hello.exe
  copied/,jenga/main.o

Artifacts are hardlinked to files in the cache (and each other)

  $ jenga build -c.
  elaborated 6 rules and 6 targets

Hardlink counts of 3 -- example,copied,.cache

  $ find ,jenga -type f | xargs stat -c "%h %n"
  3 ,jenga/example/fib.o
  3 ,jenga/example/hello.exe
  3 ,jenga/example/main.o
  3 ,jenga/copied/fib.o
  3 ,jenga/copied/hello.exe
  3 ,jenga/copied/main.o

Hardlink counts of 2 -- example,.cache

  $ rm -rf copied
  $ jenga build -c.
  elaborated 3 rules and 3 targets

  $ find ,jenga -type f | xargs stat -c "%h %n"
  2 ,jenga/example/fib.o
  2 ,jenga/example/hello.exe
  2 ,jenga/example/main.o

Hardlink counts of 1 -- example

  $ rm -rf .cache
  $ find ,jenga -type f | xargs stat -c "%h %n"
  1 ,jenga/example/fib.o
  1 ,jenga/example/hello.exe
  1 ,jenga/example/main.o

Rebuild, hardlink counts back to 2

  $ jenga build -c.
  elaborated 3 rules and 3 targets
  A: gcc -Wall -c -o fib.o fib.c
  A: gcc -Wall -c -o main.o main.c
  A: gcc -o hello.exe main.o fib.o
  ran 3 actions

  $ find ,jenga -type f | xargs stat -c "%h %n"
  2 ,jenga/example/fib.o
  2 ,jenga/example/hello.exe
  2 ,jenga/example/main.o
