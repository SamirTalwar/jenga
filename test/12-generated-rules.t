
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/example-12-generated-rules example

Build:

  $ jenga build && ,jenga/example/hello.exe
  A: cat all.files | grep '.c$' > c.files
  A: cat c.files | sed 's|\(.*\).c$|\1.o : @depends : gcc -c -o \1.o \1.c|' > c.rules
  elaborated 8 rules and 8 targets
  materalizing 5 artifacts
  A: cat all.files | grep '.h$' > h.files
  A: gcc -MG -MM $(cat c.files) > depends
  A: gcc -c -o main.o main.c
  A: gcc -c -o fib.o fib.c
  A: cat c.files | sed 's|\(.*\).c|\1.o|' > o.files
  A: gcc -o hello.exe $(cat o.files)
  ran 8 actions
  Hello, 55 jenga. Discovered deps and generated rules.

Change & rebuild:

  $ sed -i 's/10/11/' example/defs.h
  $ jenga build && ,jenga/example/hello.exe
  elaborated 8 rules and 8 targets
  materalizing 5 artifacts
  A: gcc -MG -MM $(cat c.files) > depends
  A: gcc -c -o main.o main.c
  A: gcc -o hello.exe $(cat o.files)
  ran 3 actions
  Hello, 89 jenga. Discovered deps and generated rules.

Artifacts:

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/c.rules
  ,jenga/example/depends
  ,jenga/example/hello.exe
  ,jenga/example/main.o

Artifacts (materialize all)

  $ jenga build -m
  elaborated 8 rules and 8 targets
  materalizing all targets
  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/c.rules
  ,jenga/example/c.files
  ,jenga/example/depends
  ,jenga/example/hello.exe
  ,jenga/example/o.files
  ,jenga/example/h.files
  ,jenga/example/main.o

Targets:

  $ jenga build -t
  example/all.files
  example/c.files
  example/c.rules
  example/depends
  example/fib.o
  example/h.files
  example/hello.exe
  example/main.o
  example/o.files

Rules:

  $ jenga build -r
  example/main.o : example/main.c example/fib.h example/defs.h
    gcc -c -o main.o main.c
  
  example/fib.o : example/fib.c example/fib.h
    gcc -c -o fib.o fib.c
  
  example/c.rules : example/c.files
    cat c.files | sed 's|\(.*\).c$|\1.o : @depends : gcc -c -o \1.o \1.c|' > c.rules
  
  example/depends : example/c.files example/fib.c example/main.c example/h.files example/defs.h example/fib.h
    gcc -MG -MM $(cat c.files) > depends
  
  example/h.files : example/all.files
    cat all.files | grep '.h$' > h.files
  
  example/o.files : example/c.files
    cat c.files | sed 's|\(.*\).c|\1.o|' > o.files
  
  example/c.files : example/all.files
    cat all.files | grep '.c$' > c.files
  
  example/hello.exe : example/o.files example/fib.o example/main.o
    gcc -o hello.exe $(cat o.files)
