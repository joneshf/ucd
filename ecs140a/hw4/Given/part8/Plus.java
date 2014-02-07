public class Plus {

    public static Seq plus(Seq s1, Seq s2) {
        if (s1 instanceof Constant && s2 instanceof Constant) {
            return plus((Constant) s1, (Constant) s2);
        } else if (s1 instanceof Constant && s2 instanceof Delta) {
            return plus((Constant) s1, (Delta) s2);
        } else if (s2 instanceof Constant && s1 instanceof Delta) {
            return plus((Constant) s2, (Delta) s1);
        } else if (s1 instanceof Delta && s2 instanceof Delta) {
            return plus((Delta) s1, (Delta) s2);
        } else {
            return plus((Jumble) s1, (Jumble) s2);
        }
    }

    public static Seq plus(Constant c1, Constant c2) {
        int num = Math.min(c1.num, c2.num);
        int value = c1.value + c2.value;
        return new Constant(num, value);
    }

    public static Seq plus(Delta d1, Delta d2) {
        int num = Math.min(d1.num, d2.num);
        int initial = d1.initial + d2.initial;
        // [1,2,3,4] + [2,4,6,8] = [3,6,9,12]
        // < n1 : i1 &d1 > + < n2 : i2 &d2 > =
        // [i1 + i2, (i1 + i2) + (d1 + d2), (i1 + i2) + 2(d1 + d2)]
        int delta = d1.delta + d2.delta;

        return new Delta(num, initial, delta);
    }

    public static Seq plus(Jumble j1, Jumble j2) {
        int minLength = Math.min(j1.num, j2.num);
        int[] values = new int[minLength];
        JumbleIt j1Iter = new JumbleIt(j1);
        JumbleIt j2Iter = new JumbleIt(j2);

        for (int i = 0; j1Iter.hasNext() && j2Iter.hasNext(); ++i) {
            try {
                int j1Next = j1Iter.next();
                int j2Next = j2Iter.next();
                values[i] = j1Next + j2Next;
            } catch (UsingIteratorPastEndException e) {
                break;
            }
        }

        return new Jumble(values);
    }

    public static Seq plus (Constant c, Delta d) {
        return plus(new Delta(c.num, c.value, 0), d);
    }

    public static Seq plus (Delta d, Constant c) {
        return plus(c, d);
    }

    public static Seq plus (Constant c, Jumble j) {
        int[] values = new int[c.num];

        for (int i = 0; i < values.length; ++i) {
            values[i] = c.value;
        }

        return plus(new Jumble(values), j);
    }

    public static Seq plus (Jumble j, Constant c) {
        return plus(c, j);
    }

    public static Seq plus (Delta d, Jumble j) {
        int[] values = new int[d.num];

        for (int i = 0; i < values.length; ++i) {
            values[i] = d.initial + d.delta * i;
        }

        return plus(new Jumble(values), j);
    }

    public static Seq plus (Jumble j, Delta d) {
        return plus(d, j);
    }

}
