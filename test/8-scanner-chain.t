
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example8-scanner-chain example

Initial build
  $ ./jenga.exe build --local-cache -am
  elaborated 6 rules and 6 targets
  materalizing all targets
  A: cd .jbox/0; gcc -MG -MM fib.c > fib.d
  A: cd .jbox/1; gcc -c fib.c -o fib.o
  A: cd .jbox/2; (echo -n 'main.d '; gcc -MG -MM main.c) > main.d2
  A: cd .jbox/3; gcc -MG -MM main.c > main.d
  A: cd .jbox/4; gcc -c main.c -o main.o
  A: cd .jbox/5; gcc fib.o main.o -o main.exe
  ran 6 actions

Run the executable
  $ ,jenga/example/main.exe
  hello, 55 world with #include chain

Inspect the generated deps
  $ find ,jenga -name '*.d*' | xargs grep .
  ,jenga/example/main.d:main.o: main.c fib.h defs.h defs2.h
  ,jenga/example/main.d2:main.d main.o: main.c fib.h defs.h
  ,jenga/example/fib.d:fib.o: fib.c fib.h

Mod-A (change const value)
  $ echo '#define MY_CONST 11' > example/defs2.h
  $ ./jenga.exe build --local-cache -am
  elaborated 6 rules and 6 targets
  materalizing all targets
  A: cd .jbox/0; gcc -c main.c -o main.o
  A: cd .jbox/1; gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/example/main.exe
  hello, 89 world with #include chain
  $ find ,jenga -name '*.d*' | xargs grep .
  ,jenga/example/main.d:main.o: main.c fib.h defs.h defs2.h
  ,jenga/example/main.d2:main.d main.o: main.c fib.h defs.h
  ,jenga/example/fib.d:fib.o: fib.c fib.h

Mod-B (shorten the chain)
  $ echo '#define MY_CONST 12' > example/defs.h
  $ ./jenga.exe build --local-cache -am
  elaborated 6 rules and 6 targets
  materalizing all targets
  A: cd .jbox/0; gcc -MG -MM main.c > main.d
  A: cd .jbox/1; gcc -c main.c -o main.o
  A: cd .jbox/2; gcc fib.o main.o -o main.exe
  ran 3 actions
  $ ,jenga/example/main.exe
  hello, 144 world with #include chain
  $ find ,jenga -name '*.d*' | xargs grep .
  ,jenga/example/main.d:main.o: main.c fib.h defs.h
  ,jenga/example/main.d2:main.d main.o: main.c fib.h defs.h
  ,jenga/example/fib.d:fib.o: fib.c fib.h

Mod-C (repoint the chain)
  $ echo '#define MY_CONST 13' > example/defs3.h
  $ echo '#include "defs3.h"' > example/defs.h
  $ ./jenga.exe build --local-cache -am
  elaborated 6 rules and 6 targets
  materalizing all targets
  A: cd .jbox/0; gcc -MG -MM main.c > main.d
  A: cd .jbox/1; gcc -c main.c -o main.o
  A: cd .jbox/2; gcc fib.o main.o -o main.exe
  ran 3 actions
  $ ,jenga/example/main.exe
  hello, 233 world with #include chain
  $ find ,jenga -name '*.d*' | xargs grep .
  ,jenga/example/main.d:main.o: main.c fib.h defs.h defs3.h
  ,jenga/example/main.d2:main.d main.o: main.c fib.h defs.h
  ,jenga/example/fib.d:fib.o: fib.c fib.h
