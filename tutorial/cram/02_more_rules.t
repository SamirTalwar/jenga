
This cram file accompanies the jenga tutorial.

Get a jenga executable
  $ ln $(find $TESTDIR/../../.stack-work/dist -type f -name main.exe) jenga.exe

Make a small script to run jenga with a local cache.
Avoiding interference from the global cache, which will make this test non-deterministic.

  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH

## More rules

Get the example.

  $ cp -rp $TESTDIR/../files/02/build.jenga .
  $ cp -rp $TESTDIR/../files/02/main.c .
  $ cp -rp $TESTDIR/../files/02/fib.c .

Initial build. Expect 3 actions to be run

  $ jenga build && ,jenga/hello.exe
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc -c -o main.o main.c
  A: gcc -c -o fib.o fib.c
  A: gcc -o hello.exe main.o fib.o
  ran 3 actions
  Hello, 55 jenga!
  [17]

Running executable returns exit code 17

  $ jenga build && ,jenga/hello.exe
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  Hello, 55 jenga!
  [17]

Add -Wall

  $ sed -i 's/-c -o main.o/-c -Wall -o main.o/' build.jenga
  $ cat build.jenga
  
  hello.exe : main.o fib.o
    gcc -o hello.exe main.o fib.o
  
  main.o : main.c
    gcc -c -Wall -o main.o main.c
  
  fib.o : fib.c
    gcc -c -o fib.o fib.c
  $ jenga build
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc -c -Wall -o main.o main.c
  main.c:3:6: warning: return type of 'main' is not 'int' [-Wmain]
      3 | void main() { // Oops! main should be declared to return int.
        |      ^~~~
  ran 1 action

Add -Werror to force build failure
  $ sed -i 's/-Wall/-Wall -Werror/' build.jenga
  $ jenga build
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc -c -Wall -Werror -o main.o main.c
  main.c:3:6: error: return type of 'main' is not 'int' [-Werror=main]
      3 | void main() { // Oops! main should be declared to return int.
        |      ^~~~
  cc1: all warnings being treated as errors
  jenga.exe: user action failed for rule: 'rule@5'0'
  CallStack (from HasCallStack):
    error, called at src/Engine.hs:322:7 in jenga2-0.1.0.0-Ky07jZBfm8KAUzKwZ5S1JB:Engine
  [1]

Remove -Werror againL
  $ sed -i 's/ -Werror//' build.jenga
  $ jenga build
  elaborated 3 rules and 3 targets
  materalizing 1 artifact


Fix code
  $ sed -i 's/void main/int main/' main.c
  $ cat main.c
  #include <stdio.h>
  int fib(int);
  int main() { // Oops! main should be declared to return int.
    printf("Hello, %d jenga!\n", fib(10));
  }
  $ jenga build && ,jenga/hello.exe
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc -c -Wall -o main.o main.c
  A: gcc -o hello.exe main.o fib.o
  ran 2 actions
  Hello, 55 jenga!

Define and use header file. Build fails because we failed to declare dependecy on fib.h
  $ cp -rp $TESTDIR/../files/02/fib.h .

  $ sed -i 's/int fib.*/#include "fib.h"/' main.c
  $ cat main.c
  #include <stdio.h>
  #include "fib.h"
  int main() { // Oops! main should be declared to return int.
    printf("Hello, %d jenga!\n", fib(10));
  }

  $ sed -i '1i#include "fib.h"' fib.c
  $ cat fib.c
  #include "fib.h"
  int fib(int x) {
    if (x < 2) return x;
    return fib(x-1) + fib(x-2);
  }

  $ jenga build
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc -c -Wall -o main.o main.c
  main.c:2:10: fatal error: fib.h: No such file or directory
      2 | #include "fib.h"
        |          ^~~~~~~
  compilation terminated.
  jenga.exe: user action failed for rule: 'rule@5'0'
  CallStack (from HasCallStack):
    error, called at src/Engine.hs:322:7 in jenga2-0.1.0.0-Ky07jZBfm8KAUzKwZ5S1JB:Engine
  [1]

Add missing dep to rule for main.o
  $ sed -i 's/: main.c/: main.c fib.h/' build.jenga
  $ sed -i 's/: fib.c/: fib.c fib.h/' build.jenga
  $ cat build.jenga
  
  hello.exe : main.o fib.o
    gcc -o hello.exe main.o fib.o
  
  main.o : main.c fib.h
    gcc -c -Wall -o main.o main.c
  
  fib.o : fib.c fib.h
    gcc -c -o fib.o fib.c

  $ jenga build
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  A: gcc -c -Wall -o main.o main.c
  A: gcc -c -o fib.o fib.c
  ran 2 actions
