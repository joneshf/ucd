public class ConstantIt implements SeqIt {

    protected Constant s;
    protected int count;

    public ConstantIt(Constant s) {
        this.s = s;
        this.count = 0;
    }

    public boolean hasNext() {
        return this.count < this.s.num;
    }

    public int next() {
        if (!hasNext()) {
            System.err.println("ConstantIt called past end");
            System.exit(1);
        }
        ++this.count;
        return this.s.value;
    }
}
