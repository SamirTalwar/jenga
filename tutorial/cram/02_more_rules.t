
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

Build. See one build action.
  $ jenga build
  elaborated 1 rule and 1 target
  materalizing 1 artifact
  A: gcc main.c -o hello.exe
  ran 1 action
