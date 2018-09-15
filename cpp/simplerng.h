#ifndef SIMPLERNG_H
#define SIMPLERNG_H

/**
  Implementation of a 32-bit 'Xorshift' Random Number Generator,
  based on the work of George Marsaglia (www.jstatsoft.org/v08/i14/paper , doi:10.18637/jss.v008.i14)

  NOTE: This is _not_ a cryptographically secure random number generator.
  Also, be aware that '0' should not be used as a seed, because in this case, the output of the RNG will always be '0'.

  Authors: Wiebe-Marten Wijnja (Qqwy) and Jeroen de Baat
  Date: 2018-09-15

  Usage instructions:

  1. use `SimpleRNG rng{your_lucky_number};` to seed the RNG to a desired 32-bit unsigned integer.
  2. Call `rng.rand()` to get a new random 32-bit unsigned integer.
  3. Repeat step (2) as often as you'd like.

  Re-seeding can be done using `rng.seed(some_other_number)`

 */

#include <cstdint>

class SimpleRNG
{
    uint32_t d_rng_state;
    uint32_t rand(uint32_t &rng_state);
public:
    SimpleRNG(uint32_t seed);
    void seed(uint32_t seed);
    uint32_t rand();
};

#endif

