
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example1 example

Build from clean and run:

  $ ./jenga.exe build example -a
  elaborated 3 rules and 1 root
  A: cd ,jenga/box/0; gcc -c fib.c -o fib.o
  A: cd ,jenga/box/1; gcc -c main.c -o main.o
  A: cd ,jenga/box/2; gcc fib.o main.o -o main.exe
  ran 3 actions
  $ ,jenga/artifacts/example/main.exe
  hello, 55 world

Relocate the example to a new directory - no rebuilds:

  $ mv example RELOCATED
  $ ./jenga.exe build RELOCATED -a
  elaborated 3 rules and 1 root
  $ ,jenga/artifacts/RELOCATED/main.exe
  hello, 55 world

Duplicate the example directory; build in both places - double #rules, still no rebuilds:

  $ cp -rp RELOCATED ANOTHER
  $ ./jenga.exe build RELOCATED ANOTHER -a
  elaborated 6 rules and 2 roots
  $ ,jenga/artifacts/ANOTHER/main.exe
  hello, 55 world

Modify code in one of the example directories: minimal rebuild as required. sharing when possible:

  $ sed -i 's/fib(10)/fib(20)/g' RELOCATED/main.c
  $ ./jenga.exe build RELOCATED ANOTHER -a
  elaborated 6 rules and 2 roots
  A: cd ,jenga/box/0; gcc -c main.c -o main.o
  A: cd ,jenga/box/1; gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/artifacts/RELOCATED/main.exe
  hello, 6765 world
  $ ,jenga/artifacts/ANOTHER/main.exe
  hello, 55 world
