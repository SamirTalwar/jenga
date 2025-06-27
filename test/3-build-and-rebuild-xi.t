
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example1 example

Add '-x -i flags for very detailed logging. Test will be fragile to any change in example source.

Build from clean:

  $ ./jenga.exe -a -x -i
  I: mkdir -p .cache/files
  I: mkdir -p .cache/traces
  I: rm -rf ,jenga
  I: mkdir -p ,jenga/box
  I: mkdir -p ,jenga/artifacts
  I: test -e example/config
  X: md5sum example/config
  I: test -e .cache/files/5db0ec4f4b73e5233ab712453fcb0446
  I: cp example/config .cache/files/5db0ec4f4b73e5233ab712453fcb0446
  I: chmod a-w .cache/files/5db0ec4f4b73e5233ab712453fcb0446
  I: cat example/config
  I: ls example
  elaborated 3 rules and 1 root
  I: mkdir -p .cache/files
  I: mkdir -p .cache/traces
  I: rm -rf ,jenga
  I: mkdir -p ,jenga/box
  I: mkdir -p ,jenga/artifacts
  X: md5sum example/fib.c
  I: test -e .cache/files/6c57c24ab2902d0ef0ba7ed0fc679f7f
  I: cp example/fib.c .cache/files/6c57c24ab2902d0ef0ba7ed0fc679f7f
  I: chmod a-w .cache/files/6c57c24ab2902d0ef0ba7ed0fc679f7f
  I: md5sum
  I: test -e .cache/traces/64bee4a8c7181c77b52faf24e586f7a4
  I: mkdir -p ,jenga/box/0
  I: ln .cache/files/6c57c24ab2902d0ef0ba7ed0fc679f7f ,jenga/box/0/fib.c
  A: cd ,jenga/box/0; gcc -c fib.c -o fib.o
  X: md5sum ,jenga/box/0/fib.o
  I: test -e .cache/files/47a0ee09b975f7501dbeb5431b76c24c
  I: ln ,jenga/box/0/fib.o .cache/files/47a0ee09b975f7501dbeb5431b76c24c
  I: chmod a-w .cache/files/47a0ee09b975f7501dbeb5431b76c24c
  I: rm -rf ,jenga/box/0
  I: mkdir -p .cache/traces
  I: cat> .cache/traces/64bee4a8c7181c77b52faf24e586f7a4
  X: md5sum example/main.c
  I: test -e .cache/files/b92c1d46a2f447f415b1b5ff22b473e5
  I: cp example/main.c .cache/files/b92c1d46a2f447f415b1b5ff22b473e5
  I: chmod a-w .cache/files/b92c1d46a2f447f415b1b5ff22b473e5
  I: md5sum
  I: test -e .cache/traces/f2db5cbf4ba5176303810cec83c728c4
  I: mkdir -p ,jenga/box/1
  I: ln .cache/files/b92c1d46a2f447f415b1b5ff22b473e5 ,jenga/box/1/main.c
  A: cd ,jenga/box/1; gcc -c main.c -o main.o
  X: md5sum ,jenga/box/1/main.o
  I: test -e .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: ln ,jenga/box/1/main.o .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: chmod a-w .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: rm -rf ,jenga/box/1
  I: mkdir -p .cache/traces
  I: cat> .cache/traces/f2db5cbf4ba5176303810cec83c728c4
  I: md5sum
  I: test -e .cache/traces/77ee6c82e36af75984a27ac518621689
  I: mkdir -p ,jenga/box/2
  I: ln .cache/files/47a0ee09b975f7501dbeb5431b76c24c ,jenga/box/2/fib.o
  I: ln .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1 ,jenga/box/2/main.o
  A: cd ,jenga/box/2; gcc fib.o main.o -o main.exe
  X: md5sum ,jenga/box/2/main.exe
  I: test -e .cache/files/1c3e3753bbd078e9beb1a9eeb08efdd3
  I: ln ,jenga/box/2/main.exe .cache/files/1c3e3753bbd078e9beb1a9eeb08efdd3
  I: chmod a-w .cache/files/1c3e3753bbd078e9beb1a9eeb08efdd3
  I: rm -rf ,jenga/box/2
  I: mkdir -p .cache/traces
  I: cat> .cache/traces/77ee6c82e36af75984a27ac518621689
  I: mkdir -p ,jenga/artifacts/example
  I: ln .cache/files/1c3e3753bbd078e9beb1a9eeb08efdd3 ,jenga/artifacts/example/main.exe
  ran 3 actions

Rebuild after no changes:

  $ ./jenga.exe -a -x -i
  I: mkdir -p .cache/files
  I: mkdir -p .cache/traces
  I: rm -rf ,jenga
  I: mkdir -p ,jenga/box
  I: mkdir -p ,jenga/artifacts
  I: test -e example/config
  X: md5sum example/config
  I: test -e .cache/files/5db0ec4f4b73e5233ab712453fcb0446
  I: cat example/config
  I: ls example
  elaborated 3 rules and 1 root
  I: mkdir -p .cache/files
  I: mkdir -p .cache/traces
  I: rm -rf ,jenga
  I: mkdir -p ,jenga/box
  I: mkdir -p ,jenga/artifacts
  X: md5sum example/fib.c
  I: test -e .cache/files/6c57c24ab2902d0ef0ba7ed0fc679f7f
  I: md5sum
  I: test -e .cache/traces/64bee4a8c7181c77b52faf24e586f7a4
  I: cat .cache/traces/64bee4a8c7181c77b52faf24e586f7a4
  I: test -e .cache/files/47a0ee09b975f7501dbeb5431b76c24c
  X: md5sum example/main.c
  I: test -e .cache/files/b92c1d46a2f447f415b1b5ff22b473e5
  I: md5sum
  I: test -e .cache/traces/f2db5cbf4ba5176303810cec83c728c4
  I: cat .cache/traces/f2db5cbf4ba5176303810cec83c728c4
  I: test -e .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: md5sum
  I: test -e .cache/traces/77ee6c82e36af75984a27ac518621689
  I: cat .cache/traces/77ee6c82e36af75984a27ac518621689
  I: test -e .cache/files/1c3e3753bbd078e9beb1a9eeb08efdd3
  I: mkdir -p ,jenga/artifacts/example
  I: ln .cache/files/1c3e3753bbd078e9beb1a9eeb08efdd3 ,jenga/artifacts/example/main.exe
