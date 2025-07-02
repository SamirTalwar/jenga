
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ cp -rp $TESTDIR/example-01-two-files example

Add '-x -i flags for very detailed logging. Test will be fragile to any change in example source.

Build from clean:

  $ ./jenga.exe build --local-cache -axi
  I: rm -rf .jbox
  I: rm -rf ,jenga
  I: mkdir -p .cache/files
  I: mkdir -p .cache/traces
  I: mkdir -p .jbox
  I: mkdir -p ,jenga
  I: test -e .
  I: test -d .
  I: ls .
  I: test -d jenga.exe
  I: test -d .cache
  I: test -d example
  I: test -d ,jenga
  I: test -d .jbox
  I: ls example
  I: test -d example/fib.c
  I: test -d example/main.c
  I: test -d example/make.jenga
  I: ls .jbox
  I: ls .
  I: ls .jbox
  I: ls example
  X: md5sum example/make.jenga
  I: test -e .cache/files/ade02a5a8e9b16d13646fc43aa2e61ca
  I: cp example/make.jenga .cache/files/ade02a5a8e9b16d13646fc43aa2e61ca
  I: chmod a-w .cache/files/ade02a5a8e9b16d13646fc43aa2e61ca
  I: cat example/make.jenga
  I: test -e example/main.o
  I: test -e example/fib.o
  I: test -e example/main.exe
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  X: md5sum example/fib.c
  I: test -e .cache/files/3ec221831446382d711ea3ce24237158
  I: cp example/fib.c .cache/files/3ec221831446382d711ea3ce24237158
  I: chmod a-w .cache/files/3ec221831446382d711ea3ce24237158
  I: md5sum
  I: test -e .cache/traces/347c90127bbc1461aecd80702c3c5bc4
  I: mkdir -p .jbox/0
  I: ln .cache/files/3ec221831446382d711ea3ce24237158 .jbox/0/fib.c
  A: cd .jbox/0; gcc -c fib.c -o fib.o
  X: md5sum .jbox/0/fib.o
  I: test -e .cache/files/47a0ee09b975f7501dbeb5431b76c24c
  I: ln .jbox/0/fib.o .cache/files/47a0ee09b975f7501dbeb5431b76c24c
  I: chmod a-w .cache/files/47a0ee09b975f7501dbeb5431b76c24c
  I: rm -rf .jbox/0
  I: mkdir -p .cache/traces
  I: cat> .cache/traces/347c90127bbc1461aecd80702c3c5bc4
  X: md5sum example/main.c
  I: test -e .cache/files/9d125f57501617a7e09da68a33e65d1c
  I: cp example/main.c .cache/files/9d125f57501617a7e09da68a33e65d1c
  I: chmod a-w .cache/files/9d125f57501617a7e09da68a33e65d1c
  I: md5sum
  I: test -e .cache/traces/6ce82221b5bd8437a672f4e6d19a9d9e
  I: mkdir -p .jbox/1
  I: ln .cache/files/9d125f57501617a7e09da68a33e65d1c .jbox/1/main.c
  A: cd .jbox/1; gcc -c main.c -o main.o
  X: md5sum .jbox/1/main.o
  I: test -e .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: ln .jbox/1/main.o .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: chmod a-w .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: rm -rf .jbox/1
  I: mkdir -p .cache/traces
  I: cat> .cache/traces/6ce82221b5bd8437a672f4e6d19a9d9e
  I: md5sum
  I: test -e .cache/traces/4677478c51f6bbba384ed576c9e451ed
  I: mkdir -p .jbox/2
  I: ln .cache/files/47a0ee09b975f7501dbeb5431b76c24c .jbox/2/fib.o
  I: ln .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1 .jbox/2/main.o
  A: cd .jbox/2; gcc fib.o main.o -o main.exe
  X: md5sum .jbox/2/main.exe
  I: test -e .cache/files/9efc05831ccef0c24b2697d8fff2acee
  I: ln .jbox/2/main.exe .cache/files/9efc05831ccef0c24b2697d8fff2acee
  I: chmod a-w .cache/files/9efc05831ccef0c24b2697d8fff2acee
  I: rm -rf .jbox/2
  I: mkdir -p .cache/traces
  I: cat> .cache/traces/4677478c51f6bbba384ed576c9e451ed
  I: mkdir -p ,jenga/example
  I: ln .cache/files/9efc05831ccef0c24b2697d8fff2acee ,jenga/example/main.exe
  ran 3 actions

Rebuild after no changes:

  $ ./jenga.exe build --local-cache -axi
  I: rm -rf .jbox
  I: rm -rf ,jenga
  I: mkdir -p .cache/files
  I: mkdir -p .cache/traces
  I: mkdir -p .jbox
  I: mkdir -p ,jenga
  I: test -e .
  I: test -d .
  I: ls .
  I: test -d jenga.exe
  I: test -d .cache
  I: test -d example
  I: test -d ,jenga
  I: test -d .jbox
  I: ls example
  I: test -d example/fib.c
  I: test -d example/main.c
  I: test -d example/make.jenga
  I: ls .jbox
  I: ls .
  I: ls .jbox
  I: ls example
  X: md5sum example/make.jenga
  I: test -e .cache/files/ade02a5a8e9b16d13646fc43aa2e61ca
  I: cat example/make.jenga
  I: test -e example/main.o
  I: test -e example/fib.o
  I: test -e example/main.exe
  elaborated 3 rules and 3 targets
  materalizing 1 artifact
  X: md5sum example/fib.c
  I: test -e .cache/files/3ec221831446382d711ea3ce24237158
  I: md5sum
  I: test -e .cache/traces/347c90127bbc1461aecd80702c3c5bc4
  I: cat .cache/traces/347c90127bbc1461aecd80702c3c5bc4
  I: test -e .cache/files/47a0ee09b975f7501dbeb5431b76c24c
  X: md5sum example/main.c
  I: test -e .cache/files/9d125f57501617a7e09da68a33e65d1c
  I: md5sum
  I: test -e .cache/traces/6ce82221b5bd8437a672f4e6d19a9d9e
  I: cat .cache/traces/6ce82221b5bd8437a672f4e6d19a9d9e
  I: test -e .cache/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: md5sum
  I: test -e .cache/traces/4677478c51f6bbba384ed576c9e451ed
  I: cat .cache/traces/4677478c51f6bbba384ed576c9e451ed
  I: test -e .cache/files/9efc05831ccef0c24b2697d8fff2acee
  I: mkdir -p ,jenga/example
  I: ln .cache/files/9efc05831ccef0c24b2697d8fff2acee ,jenga/example/main.exe
