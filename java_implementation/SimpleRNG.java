public class SimpleRNG {
    /**
       Implementation of a 32-bit 'Xorshift' Random Number Generator,
       based on the work of George Marsaglia (www.jstatsoft.org/v08/i14/paper , doi:10.18637/jss.v008.i14)

       NOTE: This is _not_ a cryptographically secure random number generator.
       Also, be aware that '0' should not be used as a seed, because in this case, the output of the RNG will always be '0'.

       Author: Wiebe-Marten Wijnja (Qqwy)
       Date: 2018-09-15

       Usage instructions:

       1. use `rng = new SimpleRNG(luckyNumber)` to seed the RNG to a desired 32-bit unsigned integer.
       2. Call `rng.rand()` to get a new random 32-bit unsigned integer.
       3. Repeat step (2) as often as you'd like.

       Re-seeding can be done using `rng.seed(someOtherNumber)`


       Be warned that although the library returns `longs`, the RNG will only ever output values between 0 and 2^32.
    */
    private static long max_32bit = (long) 1 << 32;

    public SimpleRNG(long seed) {
        this.seed(seed);
    }

    long rng_state;
    public long rand() {
        long num = this.rng_state;
        num ^= (num << 13) % max_32bit;
        num ^= (num >> 17) % max_32bit;
        num ^= (num << 5) % max_32bit;
        this.rng_state = num;
        return num;
    }

    public void seed(long seed) {
        this.rng_state = seed;
    }
}
