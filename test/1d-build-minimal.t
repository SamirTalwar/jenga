
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example1 example

Build from clean:

  $ ./jenga.exe build -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd ,jenga/box/0; gcc -c fib.c -o fib.o
  A: cd ,jenga/box/1; gcc -c main.c -o main.o
  A: cd ,jenga/box/2; gcc fib.o main.o -o main.exe
  ran 3 actions

Run the built artifact:

  $ ,jenga/artifacts/example/main.exe
  hello, 55 world

Update main.c "world->UNIVERSE":

  $ sed -i 's/world/UNIVERSE/g' example/main.c
  $ ./jenga.exe build -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd ,jenga/box/0; gcc -c main.c -o main.o
  A: cd ,jenga/box/1; gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/artifacts/example/main.exe
  hello, 55 UNIVERSE

Reverting to previous state of main.c causes no rebuilding:

  $ sed -i 's/UNIVERSE/world/g' example/main.c
  $ ./jenga.exe build -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  $ ,jenga/artifacts/example/main.exe
  hello, 55 world

Whitespace only change to main.c cause no link step (early cutoff):

  $ sed -i 's/int main/int      main/g' example/main.c
  $ ./jenga.exe build -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd ,jenga/box/0; gcc -c main.c -o main.o
  ran 1 action
  $ ,jenga/artifacts/example/main.exe
  hello, 55 world
