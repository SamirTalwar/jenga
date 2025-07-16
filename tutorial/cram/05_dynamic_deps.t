
Get an up-to-date jenga executable in path, which runs with a local cachee

  $ (cd $TESTDIR/../..; jenga build -q)
  $ echo exec $TESTDIR/../../,jenga/src/jenga '"$@"' --cache=. > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get the example.

  $ cp -rp $TESTDIR/../files/05 example

  $ find example
  example
  example/fib.c
  example/main.c
  example/fib.h
  example/build.jenga

Build. Expect 4 actions to be run

  $ jenga build
  elaborated 4 rules and 4 targets
  A: gcc -MG -MM *.c > depends
  A: gcc -Wall -c -o fib.o fib.c
  A: gcc -Wall -c -o main.o main.c
  A: gcc -o hello.exe main.o fib.o
  ran 4 actions

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/depends
  ,jenga/example/hello.exe
  ,jenga/example/main.o

Run the executable

  $ jenga run example/hello.exe
  Hello, 55 jenga!

See the depends

  $ cat ,jenga/example/depends
  fib.o: fib.c fib.h
  main.o: main.c fib.h

See the targets and rules

  $ jenga list-targets
  example/depends
  example/fib.o
  example/main.o
  example/hello.exe

  $ jenga list-rules
  example/depends : example/main.c example/fib.c
    cd example ; gcc -MG -MM *.c > depends
  
  example/fib.o : example/fib.c example/fib.h
    cd example ; gcc -Wall -c -o fib.o fib.c
  
  example/main.o : example/main.c example/fib.h
    cd example ; gcc -Wall -c -o main.o main.c
  
  example/hello.exe : example/main.o example/fib.o
    cd example ; gcc -o hello.exe main.o fib.o
