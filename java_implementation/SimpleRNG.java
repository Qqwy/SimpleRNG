public class SimpleRNG {
    /**
       Implementation of a 32-bit 'Xorshift' Random Number Generator,
       based on the work of George Marsaglia (www.jstatsoft.org/v08/i14/paper , doi:10.18637/jss.v008.i14)

       NOTE: This is _not_ a cryptographically secure random number generator.
       Also, be aware that '0' should not be used as a seed, because in this case, the output of the RNG will always be '0'.

       Author: Wiebe-Marten Wijnja (Qqwy)
       Date: 2018-09-15

       Usage instructions:

       1. use SimpleRNG.seed(your_lucky_number) to seed the RNG to a desired 32-bit unsigned integer.
       2. Call SimpleRNG.rand() to get a new random 32-bit unsigned integer.

       If you're working in an environment where re-entrancy is important (like when multi-threading), or want to keep track of multiple RNGs side-by-side, use `SimpleRNG.rand_r(rng_state)` insted, which will take the passed argument as RNG state (and mutate it in place to become the new state).

       (Creating a rng_state is done by calling `new SimpleRNG.RNGState(your_lucky_number)`).

       Be warned that although the library returns `longs`, the RNG will only ever output values between 0 and 2^32.
    */
    public static class RNGState {
        private long data;
        public RNGState(){
            this(42);
        }
        public RNGState(long seed) {
            this.data = seed;
        }
    }

    private static long max_32bit = (long) Math.pow(2, 32);
    public static long rand_r(SimpleRNG.RNGState rng_state) {
        long num = rng_state.data;
        num ^= (num << 13) % max_32bit;
        num ^= (num >> 17) % max_32bit;
        num ^= (num << 5) % max_32bit;
        rng_state.data = num;
        return num;
    }

    private static RNGState global_rng_state = new RNGState(42);

    public static long rand() {
        return rand_r(global_rng_state);
    }

    public static void seed(long seed) {
        global_rng_state.data = seed;
    }
}
