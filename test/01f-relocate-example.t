
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-01-two-files example

Build from clean and run:

  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: cd .jbox/0; gcc -c fib.c -o fib.o
  A: cd .jbox/1; gcc -c main.c -o main.o
  A: cd .jbox/2; gcc fib.o main.o -o main.exe
  ran 3 actions
  $ ,jenga/example/main.exe
  hello, 55 world

Relocate the example to a new directory - no rebuilds:

  $ mv example RELOCATED
  $ ./jenga.exe build --local-cache -a
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  $ ,jenga/RELOCATED/main.exe
  hello, 55 world

Duplicate the example directory; build in both places - double #rules, still no rebuilds:

  $ cp -rp RELOCATED ANOTHER
  $ ./jenga.exe build --local-cache -a
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts
  $ ,jenga/ANOTHER/main.exe
  hello, 55 world

Modify code in one of the example directories: minimal rebuild as required. sharing when possible:

  $ sed -i 's/fib(10)/fib(20)/g' RELOCATED/main.c
  $ ./jenga.exe build --local-cache -a
  elaborated 6 rules and 6 targets
  materalizing 2 artifacts
  A: cd .jbox/0; gcc -c main.c -o main.o
  A: cd .jbox/1; gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/RELOCATED/main.exe
  hello, 6765 world
  $ ,jenga/ANOTHER/main.exe
  hello, 55 world
