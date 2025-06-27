
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example1 example

Build from clean:

  $ ./jenga.exe build example -a
  elaborated 3 rules and 1 root
  A: cd ,jenga/box/0; gcc -c fib.c -o fib.o
  A: cd ,jenga/box/1; gcc -c main.c -o main.o
  A: cd ,jenga/box/2; gcc fib.o main.o -o main.exe
  ran 3 actions

Run the built artifact:

  $ ,jenga/artifacts/example/main.exe
  hello, 55 world

Update main.c "world->UNIVERSE":

  $ sed -i 's/world/UNIVERSE/g' example/main.c
  $ ./jenga.exe build example -a
  elaborated 3 rules and 1 root
  A: cd ,jenga/box/0; gcc -c main.c -o main.o
  A: cd ,jenga/box/1; gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/artifacts/example/main.exe
  hello, 55 UNIVERSE

Reverting to previous state of main.c causes no rebuilding:

  $ sed -i 's/UNIVERSE/world/g' example/main.c
  $ ./jenga.exe build example -a
  elaborated 3 rules and 1 root
  $ ,jenga/artifacts/example/main.exe
  hello, 55 world

Whitespace only change to main.c cause no link step (early cutoff):

  $ sed -i 's/int main/int      main/g' example/main.c
  $ ./jenga.exe build example -a
  elaborated 3 rules and 1 root
  A: cd ,jenga/box/0; gcc -c main.c -o main.o
  ran 1 action
  $ ,jenga/artifacts/example/main.exe
  hello, 55 world

Change config for linked executable name; cause re-link and artifact under prevous name:

  $ sed -i 's/main.exe/RENAMED.exe/g' example/config
  $ ./jenga.exe build example -a
  elaborated 3 rules and 1 root
  A: cd ,jenga/box/0; gcc fib.o main.o -o RENAMED.exe
  ran 1 action
  $ ,jenga/artifacts/example/main.exe
  /bin/sh: 32: ,jenga/artifacts/example/main.exe: not found
  [127]
  $ ,jenga/artifacts/example/RENAMED.exe
  hello, 55 world
