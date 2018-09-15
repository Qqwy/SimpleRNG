#include "simplerng.h"

uint32_t SimpleRNG::rand(uint32_t &rng_state)
{
    uint32_t num = rng_state;
    num ^= num << 13;
    num ^= num >> 17;
    num ^= num << 5;
    rng_state = num;

    return num;
}

SimpleRNG::SimpleRNG(uint32_t seed)
:
d_rng_state(seed)
{}

uint32_t SimpleRNG::rand()
{
    return rand(d_rng_state);
}

void SimpleRNG::seed(uint32_t seed)
{
    d_rng_state = seed;
}

