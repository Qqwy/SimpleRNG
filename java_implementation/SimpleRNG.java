public class SimpleRNG {

    public static class SeedState {
        private long data;
        public SeedState(){
            this(42);
        }
        public SeedState(long seed) {
            this.data = seed;
        }
    }

    private static long max_32bit = (long) Math.pow(2, 32);
    public static long rand_r(SimpleRNG.SeedState rng_state) {
        long num = rng_state.data;
        num ^= (num << 13) % max_32bit;
        num ^= (num >> 17) % max_32bit;
        num ^= (num << 5) % max_32bit;
        rng_state.data = num;
        return num;
    }

    private static SeedState global_rng_state = new SeedState(42);

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
