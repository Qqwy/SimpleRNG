public class SimpleRNG {

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

    public static void main(String[] args) {
        for(int x = 0; x < 10; ++x){
            System.out.println(Long.toString(rand()));
        }
    }
}
