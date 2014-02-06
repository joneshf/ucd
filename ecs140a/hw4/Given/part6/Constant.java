public class Constant extends Seq {
    protected int num;
    protected int value;

    public Constant(int num, int value) {
        this.num = num;
        this.value = num == 0 ? 0 : value;
    }

    public String toString() {
        return "[ "+this.num+" : "+this.value+" ]";
    }

    public int min() {
        return this.value;
    }

    public SeqIt createSeqIt() {
        return new ConstantIt(this);
    }
}
