#include <stdio.h>


int example(int v) {
  printf("%d\n", v);
  return v;
}

int main(int argc, const char *argv[])
{
  int (*fp)(int) = example;
  (*fp)(2);
  return 0;
}
