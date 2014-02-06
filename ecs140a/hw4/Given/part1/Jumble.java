public class Jumble extends Seq {
    protected int[] values;

    public Jumble(int[] values) {
        this.values = new int[values.length];
        System.arraycopy(values, 0, this.values, 0, values.length);
    }

    public String toString() {
        // Grossly inefficient in theory, but hopefully the compiler is smart.
        String str = "{ "+this.values.length+" : ";
        for (int value : values) {
            str += value+" ";
        }
        return str+"}";
    }
}
