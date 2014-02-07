public class Delta extends Seq {
    protected int num;
    protected int initial;
    protected int delta;

    public Delta(int num, int initial, int delta) {
        this.num = num;
        this.initial = num == 0 ? 0 : initial;
        this.delta = num == 0 ? 0 : delta;
    }

    public String toString() {
        return "< "+this.num+" : "+this.initial+" &"+this.delta+" >";
    }

    public int min() {
        return Math.min(this.initial, this.initial + this.delta * (this.num - 1));
    }

    public SeqIt createSeqIt() {
        return new DeltaIt(this);
    }
}
