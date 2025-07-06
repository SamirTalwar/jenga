
This cram file accompanies the jenga tutorial document.

Get a jenga executable
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe

Make a small script to run jenga with a local cache.
Avoiding interference from the global cache, which will make this test non-deterministic.

  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH

## Step1: First build

Get the example.
  $ cp -rp $TESTDIR/files/build.jenga .
  $ cp -rp $TESTDIR/files/main.c .

Build. See one build action.
  $ jenga build
  elaborated 1 rule and 1 target
  materalizing 1 artifact
  A: gcc main.c -o hello.exe
  ran 1 action

Zero rebuild. See no build actions.
  $ jenga build
  elaborated 1 rule and 1 target
  materalizing 1 artifact

Run built executable. See no actions, then the hello program output.
# $ jenga run hello.exe # TODO
  $ jenga build; ,jenga/hello.exe
  elaborated 1 rule and 1 target
  materalizing 1 artifact
  Hello, jenga world!

Modify file and rebuild. See the rebuild action, and the changed output.
  $ sed -i s/world/universe/ main.c
  $ jenga build; ,jenga/hello.exe
  elaborated 1 rule and 1 target
  materalizing 1 artifact
  A: gcc main.c -o hello.exe
  ran 1 action
  Hello, jenga universe!

Reset file. See no actions (they were cached), but the output reverted.
  $ sed -i s/universe/world/ main.c
  $ jenga build; ,jenga/hello.exe
  elaborated 1 rule and 1 target
  materalizing 1 artifact
  Hello, jenga world!


## Step2: More rules; more source files.

TODO
