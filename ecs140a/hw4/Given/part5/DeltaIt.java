public class DeltaIt implements SeqIt {

    protected Delta s;
    protected int count;

    public DeltaIt(Delta s) {
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
        return this.s.initial + this.s.delta * this.count++;
    }
}
