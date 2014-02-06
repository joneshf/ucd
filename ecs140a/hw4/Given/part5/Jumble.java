public class Jumble extends Seq {
    protected int num;
    protected int[] values;

    public Jumble(int[] values) {
        this.num = values.length;
        this.values = new int[this.num];
        System.arraycopy(values, 0, this.values, 0, values.length);
    }

    public String toString() {
        // Grossly inefficient in theory, but hopefully the compiler is smart.
        String str = "{ "+this.num+" : ";
        for (int value : values) {
            str += value+" ";
        }
        return str+"}";
    }

    public int min() {
        if (this.num == 0) {
            return 0;
        } else {
            int minimum = this.values[0];

            for (int value : this.values) {
                minimum = value < minimum ? value : minimum;
            }

            return minimum;
        }
    }

    public SeqIt createSeqIt() {
        return new JumbleIt(this);
    }
}
