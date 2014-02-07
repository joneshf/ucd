public class Plus {

    private enum SeqType {
        CONSTANT,
        DELTA,
        JUMBLE
    }

    private static Seq plusJumble(int num, Seq s1, Seq s2) {
        SeqIt s1Iter = s1.createSeqIt();
        SeqIt s2Iter = s2.createSeqIt();
        int[] values = new int[num];

        try {
            for (int i = 0; i < num; ++i) {
                values[i] = s1Iter.next() + s2Iter.next();
            }
        } catch (UsingIteratorPastEndException e) {
        }

        return new Jumble(values);
    }

    public static Seq plus(Seq s1, Seq s2) {
        // We're going to iterate the sequences,
        // keeping track of the number of elements, their differences, and so on.
        // If we get to the end and the difference has been 0 the whole time,
        // It's a Constant.
        // If the difference stays the same, but isn't 0, it's a Delta.
        // If at any point the difference changes between two additions,
        // it's a Jumble.
        SeqIt s1Iter = s1.createSeqIt();
        SeqIt s2Iter = s2.createSeqIt();

        int num = 0;
        int initial = 0;
        int diff = 0;
        int last = 0;
        SeqType type = SeqType.CONSTANT;

        try {
            for (; ; ++num) {
                int s1Next = s1Iter.next();
                int s2Next = s2Iter.next();
                if (num == 0) {
                    // This is the first one.
                    initial = s1Next + s2Next;
                } else if (num == 1) {
                    // We can check the diff now.
                    diff = s1Next + s2Next - initial;
                    type = diff == 0 ? SeqType.CONSTANT : SeqType.DELTA;
                } else if (diff != s1Next + s2Next - last) {
                    // This thing is a jumble.
                    type = SeqType.JUMBLE;
                }
                last = s1Next + s2Next;
            }
        } catch (UsingIteratorPastEndException e) {
        }

        switch (type) {
            case CONSTANT: return new Constant(num, initial);
            case DELTA: return new Delta(num, initial, diff);
            default: return plusJumble(num, s1, s2);
        }
    }

}
