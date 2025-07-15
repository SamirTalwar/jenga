
# Build artifacts and the build cache

This section explores how Jenga manages build artifacts and its cache of previous builds.

- [Here](files/03) are the files used in this section of the tutorial.
- [Here](cram/03_artifacts_and_cache.t) is the companion cram file.

The examples continues from where we reached at the end of section 2.
We have 3 source files.

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

And `build.jenga` contains 3 rules:
```
hello.exe : main.o fib.o
  gcc -o hello.exe main.o fib.o

main.o : main.c fib.h
  gcc -Wall -c -o main.o main.c

fib.o : fib.c fib.h
  gcc -Wall -c -o fib.o fib.c
```

We work in a fresh directory, with the above 4 files in a sub-directory `example`.
```
$ find .
.
./example
./example/fib.c
./example/main.c
./example/fib.h
./example/build.jenga
```

Previously we `cd`ed into the example directory containing the source files before running a jenga build;
but this is not necessary.

We can build in a parent directory, and jenga will search recursively for all `build.jenga` files, and build all defined targets or bring them up to date.

If you previously worked through last section of the tutorial, Jenga will not have to run any actions.
```
$ jenga build
elaborated 3 rules and 3 targets
```

If you didn't run jenga before, or you have deleted Jenga's cache (more on this soon), then you will see 3 action being run.
But repeating the `jenga build` command a second time will do no further work.

```
$ jenga build
elaborated 3 rules and 3 targets
A: gcc -Wall -c -o fib.o fib.c
A: gcc -Wall -c -o main.o main.c
A: gcc -o hello.exe main.o fib.o
ran 3 actions

$ jenga build
elaborated 3 rules and 3 targets
```

## Build Artifacts (`,jenga/`)

The built artifacts will be found under the directory`,jenga`, which has the same directory sub-structure as the source directories.
```
$ find ,jenga
,jenga/
,jenga/example
,jenga/example/fib.o
,jenga/example/hello.exe
,jenga/example/main.o
```

If we `cd` into the `example` directory and build there, we get a different `,jenga` directory without the sub-structure. But note again, no build actions were actually run.
```
$ cd example
$ jenga build
elaborated 3 rules and 3 targets
$ find ,jenga
,jenga/
,jenga/fib.o
,jenga/hello.exe
,jenga/main.o
$ cd ..
```


## Build cache (`.cache/jenga`)

By default, Jenga's cache is stored at `$HOME/.cache/jenga`.

It is safe to blow away this jenga cache at any time (`rm -rf ~/.cache/jenga`), but there is no reason to do this.

To explore how the caching works, we can ask Jenga to build using an alternative cache location. For example, we might select `--cache=/tmp`, and assuming this cache did not previously exist, Jenga is forced to run the 3 build actions.
```
$ rm -rf /tmp/.cache/jenga
$ jenga --cache=/tmp
elaborated 3 rules and 3 targets
A: gcc -Wall -c -o fib.o fib.c
A: gcc -Wall -c -o main.o main.c
A: gcc -o hello.exe main.o fib.o
ran 3 actions
```

Running the build again with the same cache location runs no actions.
```
$ jenga --cache=/tmp
elaborated 3 rules and 3 targets
```

## Build cache (files) (`.cache/jenga/files`)

A jenga cache has two sub-directories, `files` and `traces`.
Here is the contents of the files sub-directory.
```
$ find /tmp/.cache/jenga/files
/tmp/.cache/jenga/files
/tmp/.cache/jenga/files/ac0905e8c3e0f576c7368927afbe7cb3
/tmp/.cache/jenga/files/83f35fc3965c22be4e45c16356b74c5b
/tmp/.cache/jenga/files/0fcce4811e995a71fe45c2826bb0868b
/tmp/.cache/jenga/files/47a0ee09b975f7501dbeb5431b76c24c
/tmp/.cache/jenga/files/3f76f8b56f5f210a58391a85a90df90c
/tmp/.cache/jenga/files/2360cef3c9dd4578f441193f7fd17242
/tmp/.cache/jenga/files/2b669a2f7d781171abadfc53ff38d0fd
```

The `files` sub-directory contains all source files and generated targets, for every build we have done which used this cache.

For example, here is our `fib.h` source file:
```
$ cat /tmp/.cache/jenga/files/2360cef3c9dd4578f441193f7fd17242
int fib(int);
```

Each file is named by its `md5sum`. This is how jenga's caching works.
We can verify this if we like.
```
$ md5sum /tmp/.cache/jenga/files/2360cef3c9dd4578f441193f7fd17242
2360cef3c9dd4578f441193f7fd17242  /tmp/.cache/jenga/files/2360cef3c9dd4578f441193f7fd17242
```

One of the files in the cache is the `main.exe` which jenga built for us.
If we wanted we could figure out which. (Although there is no need to to do this, this is simply for exposition).

Compute the md5sum of the executable in the artifacts directory.
```
$ md5sum ,jenga/example/hello.exe
0fcce4811e995a71fe45c2826bb0868b  ,jenga/example/hello.exe
```
And then we could even execute directly from the cache:
```
$ /tmp/.cache/jenga/files/0fcce4811e995a71fe45c2826bb0868b
Hello, 55 jenga!
```

## Hard links (Too much information)

Jenga builds work by creating hard links between the build artifacts and the cached files:
We can see this by examining the _link count_ (field 2) of the `ls -l` listings of the cache and artifact files. The three cache files which correspond to the artifacts have link count of 2.
```
$ find /tmp/.cache/jenga/files -type f | xargs ls -l
-r-xr-xr-x 2 nic nic 7904 Jul 15 10:31 /tmp/.cache/jenga/files/0fcce4811e995a71fe45c2826bb0868b
-r--r--r-- 1 nic nic   14 Jul 15 10:31 /tmp/.cache/jenga/files/2360cef3c9dd4578f441193f7fd17242
-r--r--r-- 1 nic nic   89 Jul 15 10:31 /tmp/.cache/jenga/files/2b669a2f7d781171abadfc53ff38d0fd
-r--r--r-- 1 nic nic   92 Jul 15 10:31 /tmp/.cache/jenga/files/3f76f8b56f5f210a58391a85a90df90c
-r--r--r-- 2 nic nic 1272 Jul 15 10:31 /tmp/.cache/jenga/files/47a0ee09b975f7501dbeb5431b76c24c
-r--r--r-- 2 nic nic 1448 Jul 15 10:31 /tmp/.cache/jenga/files/83f35fc3965c22be4e45c16356b74c5b
-r--r--r-- 1 nic nic  164 Jul 15 10:31 /tmp/.cache/jenga/files/ac0905e8c3e0f576c7368927afbe7cb3

$ find ,jenga -type f | xargs ls -l
-r--r--r-- 2 nic nic 1272 Jul 15 10:31 ,jenga/fib.o
-r-xr-xr-x 2 nic nic 7904 Jul 15 10:31 ,jenga/hello.exe
-r--r--r-- 2 nic nic 1448 Jul 15 10:31 ,jenga/main.o
```

If we blow away our temporary cache then the artifacts still exist, just with a reduced link couunt.
```
$ rm -rf /tmp/.cache/jenga/
$ find ,jenga -type f | xargs ls -l
-r--r--r-- 1 nic nic 1272 Jul 15 10:31 ,jenga/fib.o
-r-xr-xr-x 1 nic nic 7904 Jul 15 10:31 ,jenga/hello.exe
-r--r--r-- 1 nic nic 1448 Jul 15 10:31 ,jenga/main.o
```


## Build cache (traces) (`.cache/jenga/traces`)

The other component of a build cache are the traces.
```
$ find /tmp/.cache/jenga/traces
/tmp/.cache/jenga/traces
/tmp/.cache/jenga/traces/a6ee54a51d20555c82690e59023b2b97
/tmp/.cache/jenga/traces/03e3513fd450744bf3ed4382d15c27b7
/tmp/.cache/jenga/traces/708d2bd7c6f00b297df20d13e4c574a6
```

These record the effect of every build action jenga has run against this cache. They form a link between the message digest (`md5sum`) of the inputs and output an action. There is no reason to inspect these other than to understand how the jenga build system works. Lets look at one:

```
$ cat /tmp/.cache/jenga/traces/a6ee54a51d20555c82690e59023b2b97
WIT {command = "gcc -o hello.exe main.o fib.o", deps = [("fib.o","47a0ee09b975f7501dbeb5431b76c24c"),("main.o","83f35fc3965c22be4e45c16356b74c5b")], targets = [("hello.exe","0fcce4811e995a71fe45c2826bb0868b")]}
```

This trace records Jenga's knowledge of a specific instance of the _link_ command that has been run. It says that if this specific command is run with this specific set of input files (`deps`) with specific digests, then the build action will generate an output file (`targets`) with a specific name and digest.

The traces are how jenga can avoid running build actions which have been run before.

Continue the jenga tutorial by learning how to
[control jenga builds at the command line](04_options.md).
