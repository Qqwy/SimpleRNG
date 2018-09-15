public class Example {
    public static void main(String[] args) {
        SimpleRNG rng = new SimpleRNG(42);
        for(int x = 0; x < 10; ++x){
            System.out.println(Long.toString(rng.rand()));
        }
    }
}
