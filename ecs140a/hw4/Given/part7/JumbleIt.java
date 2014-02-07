public class JumbleIt implements SeqIt {

    protected Jumble s;
    protected int count;

    public JumbleIt(Jumble s) {
        this.s = s;
        this.count = 0;
    }

    public boolean hasNext() {
        return this.count < this.s.values.length;
    }

    public int next() throws UsingIteratorPastEndException {
        if (!hasNext()) {
            throw new UsingIteratorPastEndException("");
        }
        return this.s.values[this.count++];
    }
}
