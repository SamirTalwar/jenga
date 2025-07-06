
# jenga tutorial

Following a successful download and install of jenga, this tutorial will guide you through the first steps in writing jenga rules and running jenga builds.

## Download, build and install jenga

The following steps will download and build `jenga` from source.
```
rm -rf /tmp/jenga2
cd /tmp
git clone git@github.com:Nick-Chapman/jenga2.git
cd jenga2/
stack build
cram doc/tutorial.t
```

This tutorial assumes you have `jenga` in your path.
For example, you might choose to link a file `jenga` in a directory picked up by your path to the executable built by stack above.
```
cd ~/.local/bin
ln -s $(find /tmp/jenga2/.stack-work/dist -type f -name main.exe) jenga
```

Now we can display jenga's top level usage/help message.
```
jenga --help
```

Alongside this tutorial is a companion [cram test file](tutorial.t). This records the various steps of the tutorial alongside the expected output. If this tutorial diverges from the cram file, then the cram file should be regarded as the source of truth for how jenga operates, because it gets run/checked as part of the above jenga install. If you don't want to run the steps of this tutorial yourself, you could just browse the cram file to see exactly what happens at each step.

## Step 1: First build

Let's use jenga to build and run a simple C application.

Here's our single source file `main.c`.
```
#include <stdio.h>
int main() {
  printf("hello, jenga!\n");
}
```
And here's our single build rule in the file `build.jenga`.
```
hello.exe : main.c
  gcc main.c -o hello.exe
```

Let's work in a fresh directory to try this out.
First type or copy in the example files.
Then run `jenga build` in the new directory.
```
mkdir /tmp/j
cd /tmp/j
cp /tmp/jenga2/doc/files/main.c .
cp /tmp/jenga2/doc/files/build.jenga .
jenga build
```

Jenga will respond:
```
elaborated 1 rule and 1 target
materalizing 1 artifact
A: gcc main.c -o hello.exe
ran 1 action
```

Now let's run our executable which has been placed in the `,jenga/` artifacts directory.
```
# jenga run hello.exe # TODO
jenga build; ,jenga/hello.exe
```
And see the output.
```
Hello, jenga world!
```

Let's make a change to our source code.
Perhaps edit the `main.c` to replace the string `world` with `universe`.
Rebuild and rerun.
Jenga will rerun the `gcc` action because the input file `main.c` has changed.
```
elaborated 1 rule and 1 target
materalizing 1 artifact
A: gcc main.c -o hello.exe
ran 1 action
Hello, jenga universe!
```

Revert the example back to its original state, replacing `universe` with `world`.
Rebuild and rerun again.
This time, no build actions are run. Yet the executable reverts to its original behaviour. This is a result of _full caching_ which will be discussed more in the following steps of the tutorial.
```
elaborated 1 rule and 1 target
materalizing 1 artifact
Hello, jenga world!
```

## Step2: More rules; more source files.

TODO
