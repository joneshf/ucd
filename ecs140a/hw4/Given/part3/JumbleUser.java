public class JumbleUser {

    public static int lengthLongestNDCSS1(Jumble j) {
        JumbleIt jIter = new JumbleIt(j);
        int maxLongest = 0;
        int longest = 0;
        int last = 0;

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

        return Math.max(maxLongest, longest);
    }
}
