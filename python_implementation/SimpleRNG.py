"""
  Implementation of a 32-bit 'Xorshift' Random Number Generator,
  based on the work of George Marsaglia (www.jstatsoft.org/v08/i14/paper , doi:10.18637/jss.v008.i14)

  NOTE: This is _not_ a cryptographically secure random number generator.
  Also, be aware that '0' should not be used as a seed, because in this case, the output of the RNG will always be '0'.

  Author: Wiebe-Marten Wijnja (Qqwy)
  Date: 2018-09-15

  Usage instructions:

  1. use `rng = SimpleRNG(your_lucky_number)` to seed the RNG to a desired 32-bit unsigned integer.
  2. Call `rng.rand()` to get a new random 32-bit unsigned integer.
  3. Repeat step (2) as often as you'd like.

  Be warned that although Python uses arbitrary large integers, this RNG will only ever output values between 0 and 2^32.
"""

class SimpleRNG(object):
    def __init__(self, seed):
        self.seed(seed)

    def seed(self, seed):
        self.rng_state = seed

    def rand(self):
        num = self.rng_state
        num ^= (num << 13) % (2 ** 32)
        num ^= (num >> 17) % (2 ** 32)
        num ^= (num << 5) % (2 ** 32)
        self.rng_state = num
        return num

# Example:
if __name__ == '__main__':
    rng = SimpleRNG(42)
    for x in range(0, 10):
        print(rng.rand())
