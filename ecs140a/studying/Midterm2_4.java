public class Midterm2_4 {

    public static void main(String[] args) {
        mystery(new CharSeq("a"));
        mystery(new CharSeq("ab"));
        mystery(new CharSeq("babcaa"));
        mystery(new CharSeq("aaaaabaaaaaaaa"));
        mystery(new CharSeq("rirggiirhtttrr"));
        mystery2(new CharSeq("a"));
        mystery2(new CharSeq("ab"));
        mystery2(new CharSeq("babcaa"));
        mystery2(new CharSeq("aaaaabaaaaaaaa"));
        mystery2(new CharSeq("rirggiirhtttrr"));
    }

    public static class CharSeq {
        public char[] seq;

        public CharSeq(String s) {
            int strLen = s.length();
            this.seq = new char[strLen];
            for (int i = 0; i < strLen; ++i) {
                this.seq[i] = s.charAt(i);
            }
        }
    }

    public static class CharSeqIt {

        protected int length;
        protected int it;
        protected char[] cs;

        public CharSeqIt(final CharSeq cs) {
            this.length = cs.seq.length;
            this.cs = cs.seq;
            this.it = 0;
        }

        public boolean hasNext() {
            return this.it != this.length;
        }

        public char next() {
            return this.cs[this.it++];
        }
    }

    protected static void mystery(CharSeq s) {
        CharSeqIt i1 = new CharSeqIt(s);
        int pos1 = 0;

        while (i1.hasNext()) {
            char p1 = i1.next();
            CharSeqIt i2 = new CharSeqIt(s);
            boolean OK = true;
            int pos2 = 0;

            while (pos2 < pos1) {
                char p2 = i2.next();

                if (p1 == p2) {
                    OK = false;
                    break;
                }

                pos2++;
            }

            if (OK) {
                System.out.print(p1);
            }
            pos1++;
        }
        System.out.println();
    }

    protected static void mystery2(final CharSeq s) {
        CharSeqIt i1 = new CharSeqIt(s);
        int pos1 = 0;

        try {
            while (true) {
                char p1 = i1.next();
                CharSeqIt i2 = new CharSeqIt(s);
                boolean OK = true;
                int pos2 = 0;

                while (pos2 < pos1) {
                    char p2 = i2.next();

                    if (p1 == p2) {
                        OK = false;
                        break;
                    }

                    pos2++;
                }

                if (OK) {
                    System.out.print(p1);
                }
                pos1++;
            }
        } catch (Exception e) {

        }
        System.out.println();
    }
}
