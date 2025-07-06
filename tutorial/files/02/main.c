#include <stdio.h>
int fib(int);
void main() { // Oops! main should be declared to return int.
  printf("Hello, %d jenga!\n", fib(10));
}
