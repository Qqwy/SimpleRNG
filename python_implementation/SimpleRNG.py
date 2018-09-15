"""
  Implementation of a 32-bit 'Xorshift' Random Number Generator,
  based on the work of George Marsaglia (www.jstatsoft.org/v08/i14/paper , doi:10.18637/jss.v008.i14)

  NOTE: This is _not_ a cryptographically secure random number generator.
  Also, be aware that '0' should not be used as a seed, because in this case, the output of the RNG will always be '0'.

  Author: Wiebe-Marten Wijnja (Qqwy)
  Date: 2018-09-15

  Usage instructions:

  1. use SimpleRNG.seed(your_lucky_number) to seed the RNG to a desired 32-bit unsigned integer.
  2. Call SimpleRNG.rand() to get a new random 32-bit unsigned integer.

  If you're working in an environment where re-entrancy is important (like when multi-threading), or want to keep track of multiple RNGs side-by-side,use `SimpleRNG.rand_r(rng_state)` instead, which will take the passed argument as RNG state (and mutate it in place to become the new state).


  Be warned that although Python uses arbitrary large integers, this RNG will only ever output values between 0 and 2^32.
"""

class SeedState(object):
    def __init__(self, seed = 42):
        self.data = seed

def rand_r(rng_state):
    num = rng_state.data
    num ^= (num << 13) % (2 ** 32)
    num ^= (num >> 17) % (2 ** 32)
    num ^= (num << 5) % (2 ** 32)
    rng_state.data = num
    return num


__global_rng_state = RNGState(42)

def rand():
    global __global_rng_state
    return rand_r(__global_rng_state)

def seed(seed):
    global __global_rng_state
    __global_rng_state = SeedState(seed)

if __name__ == '__main__':
    for x in range(0, 10):
        print(rand())
