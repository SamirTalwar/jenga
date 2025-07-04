
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-07-scanner-deps example

Initial build
  $ ./jenga.exe build -c. -am
  elaborated 5 rules and 5 targets
  materalizing all targets
  A: gcc -MG -MM fib.c > fib.d
  A: gcc -c fib.c -o fib.o
  A: gcc -MG -MM main.c > main.d
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 5 actions

Run the executable
  $ ,jenga/example/main.exe
  hello, 55 world with scanner deps

Inspect the generated deps
  $ find ,jenga -name '*.d' | xargs cat
  main.o: main.c fib.h defs.h
  fib.o: fib.c fib.h

  $ echo '#define MY_CONST 11' > example/defs.h
  $ ./jenga.exe build -c. -a
  elaborated 5 rules and 5 targets
  materalizing 1 artifact
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/example/main.exe
  hello, 89 world with scanner deps
