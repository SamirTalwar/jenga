#include <stdio.h>
#include "fib.h"
#include "defs.h"
int fib(int);
int main() {
  printf("hello, %d world with combined cc and make configs\n",fib(MY_CONST));
}
