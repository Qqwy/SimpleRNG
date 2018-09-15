#include <stdio.h>

#include "simple_rng.c"

int main() {

  for(size_t x = 0; x < 10; ++x) {
    printf("%lu\n", (unsigned long)SimpleRNG_rand());
  }
}
