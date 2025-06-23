//#include "fib.h" //TODO: mmake this work
int fib(int x) {
  if (x < 2) return x;
  return fib(x-1) + fib(x-2);
}
