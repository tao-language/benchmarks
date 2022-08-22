#include <stdio.h>

long fib(long n) {
  if (n <= 1)
    return n;
  return fib(n - 1) + fib(n - 2);
}

int main() {
  printf("%ld", fib(36));
  return 0;
}