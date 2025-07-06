
# More rules and more source files

This tutorial section dives deeper into what constitutes a jenga build rule.
We see examples with multiple rules and multiple source files.

- [Here](files/02) are the files used in this section of the tutorial.
- [Here](cram/02_more_rules.t) is the companion cram file.

This example starts with two `.c` files and a `build.jenga` file containing three rules.

main.c
```
#include <stdio.h>
int fib(int);
void main() { // Oops! main should be declared to return int.
  printf("Hello, %d jenga!\n", fib(10));
}
```

fib.c
```
int fib(int x) {
  if (x < 2) return x;
  return fib(x-1) + fib(x-2);
}
```

build.jenga
```
hello.exe : main.o fib.o
  gcc -o hello.exe main.o fib.o

main.o : main.c
  gcc -c -o main.o main.c

fib.o : fib.c
  gcc -c -o fib.o fib.c
```

These build rules implement the standard approach to separate compilation and linking.
Each `.c`-file is independently compile to a `.o`-file using `gcc -c`. Then the two `.o`-files are linked together using plain `gcc`.

Note how each rule explicitly lists its dependencies. Any file which is required to run the action must be listed. If any dependencies are missing, then the compilation and hence the entire build will fail.

This is a good thing! Build systems which don't setup explicit sandboxes for running build action may suffer from _accidental_ build success. This is a very bad thing. It means than if a file changes which was read by a build step but not explicitly listed as a dependency, then a rebuild may fail to rerun the action, leading to incorrect builds. The developer nightmare!

Let's build and run this example:
```
jenga build && ,jenga/hello.exe
```

Jenga responds by running the three actions in the order determined by the dependencies.
```
$ jenga build
elaborated 3 rules and 3 targets
materalizing 1 artifact
A: gcc -c -o main.o main.c
A: gcc -c -o fib.o fib.c
A: gcc -o hello.exe main.o fib.o
ran 3 actions
Hello, 55 jenga!
```

Unfortunately, not all is well. If we look at the exit code from running our executable we see something strange:

```
,jenga/hello.exe; echo $?
Hello, 55 jenga!
17
```

Turns out there is a bug in our `main.c`
In fact we even seem to have commented our code with that fact :)
We'll fix that soon.
But first lets switch up our compiler warnings so we get notified about our mistake.
We'll change just the rules for `main.o`:
```
main.o : main.c
  gcc -c -Wall -o main.o main.c
```

Now we rerun the build. jenga know that even though we didn't modify any source file, the action for generating `main.o` has changed and so the rule must be trigged:

```
$ jenga build
elaborated 3 rules and 3 targets
materalizing 1 artifact
A: gcc -c -Wall -o main.o main.c
main.c:3:6: warning: return type of 'main' is not 'int' [-Wmain]
    3 | void main() { // Oops! main should be declared to return int.
      |      ^~~~
ran 1 action

```

Jenga has run the replacement action for `main.o`.
The C compiler has pointed out the mistake.
Jenga has not rerun the link step, so we can surmise that the generated `main.o` file is unchanged.

If we wanted, we could add `-Werror` as well as `-Wall` which would cause the C compiler to produce a non-zero exit code.
This would be detcted by jenga and treated as a build failure.

But lets fix the source code, rebuild and move on.
Change `main.c` to read:
```
#include <stdio.h>
int fib(int);
int main() {
  printf("Hello, %d jenga!\n", fib(10));
}
```

Rebuild. Jenga must rerun the compile step for the changed `main.c` and the link step (`main.o` will have changed):
```
$ jenga build && ,jenga/hello.exe
elaborated 3 rules and 3 targets
materalizing 1 artifact
A: gcc -c -Wall -Werror -o main.o main.c
A: gcc -o hello.exe main.o fib.o
ran 2 actions
Hello, 55 jenga!
```


Let make one final change to this example.
It's common practise in C-code to `#include` a header providing the prototype for externally defined functions, just as the call to `fib(10)` in `main.c`
Currently in our example, we have `main.c` directly state the prototype: `int fib(int)`.
We shall move this to a new file, `fib.h` which can be included in `main.c` and '`fib.c`
We include it in `fib.c` so the compiler can check there is no mismatch between the declaration and the definition of `fib`.

To be precise, here is what our source files should look like now:

fib.h
```
int fib(int);
```

main.c
```
#include <stdio.h>
#include "fib.h"
int main() {
  printf("Hello, %d jenga!\n", fib(10));
}
```

fib.c
```
#include "fib.h"
int fib(int x) {
  if (x < 2) return x;
  return fib(x-1) + fib(x-2);
}
```

Unfortunately, when we try to build, jenga will error:
```
$ jenga build
elaborated 3 rules and 3 targets
materalizing 1 artifact
A: gcc -c -Wall -Werror -o main.o main.c
main.c:2:10: fatal error: fib.h: No such file or directory
    2 | #include "fib.h"
      |          ^~~~~~~
compilation terminated.
jenga.exe: user action failed for rule: 'rule@5'0'
```

The strange notation `rule@5'0` tells us the failure occurred when running the rule defined at line 5, column 0 of `build.jenga`
Jenga has failed because we failed to list `fib.h` as a dependency of the rule which builds `main.o`.

Running in a sandbox has not allowed accidental build success. The file `fib.h` is simply not available to be read by the compiler.

We need to fix out build rules to list the dependency on `fib.h`. We have two places to fix:
`build.jenga` now looks like this:
```
hello.exe : main.o fib.o
  gcc -o hello.exe main.o fib.o

main.o : main.c fib.h
  gcc -c -Wall -Werror -o main.o main.c

fib.o : fib.c fib.h
  gcc -c -o fib.o fib.c
```

Building one last time with jenga reports:
```
$ jenga build
elaborated 3 rules and 3 targets
materalizing 1 artifact
A: gcc -c -Wall -o main.o main.c
A: gcc -c -o fib.o fib.c
ran 2 actions
```
