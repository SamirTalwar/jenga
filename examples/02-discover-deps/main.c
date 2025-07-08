#include <stdio.h>
#include "fib.h"
#include "defs.h"
int fib(int);
int main() {
  printf("hello, %d world with auto discovery\n",fib(MY_CONST));
}
