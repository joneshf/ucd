public class JumbleUser {

    public static int lengthLongestNDCSS1(Jumble j) {
        JumbleIt jIter = new JumbleIt(j);
        int maxLongest = 0;
        int longest = 0;
        int last = 0;

        try {
            while (jIter.hasNext()) {
                    int next = jIter.next();
                if (longest == 0 || last <= next) {
                    ++longest;
                } else if (longest > maxLongest) {
                    maxLongest = longest;
                    longest = 1;
                } else {
                    longest = 1;
                }
                last = next;
            }
        } catch (UsingIteratorPastEndException e) {
        }

        return Math.max(maxLongest, longest);
    }

    public static int lengthLongestNDCSS2(Jumble j) {
        JumbleIt jIter = new JumbleIt(j);
        int maxLongest = 0;
        int longest = 0;
        int last = 0;

        while (true) {
            try {
                int next = jIter.next();
                if (longest == 0 || last <= next) {
                    ++longest;
                } else if (longest > maxLongest) {
                    maxLongest = longest;
                    longest = 1;
                } else {
                    longest = 1;
                }
                last = next;
            } catch (UsingIteratorPastEndException e) {
                break;
            }
        }

        return Math.max(maxLongest, longest);
    }
}
