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

    public int next() throws UsingIteratorPastEndException {
        if (!hasNext()) {
            throw new UsingIteratorPastEndException("");
        }
        ++this.count;
        return this.s.value;
    }
}
