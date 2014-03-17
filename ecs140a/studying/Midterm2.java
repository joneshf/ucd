public class Midterm2 {

    public static class A {
        int brandon;
        String hardy;

        public A(int b, String h) {
            this.brandon = b;
            this.hardy = h;
        }

        public String toString() {
            return "A("+this.brandon+", "+this.hardy+")";
        }
    }

    public static class B extends A {
        int cee;

        public B(int b, String h, int c) {
            super(b, h);
            this.cee = c;
        }
        public String toString() {
            return "B("+this.brandon+", "+this.hardy+","+this.cee+")";
        }
    }

    public static class C extends A {
        String dee;

        public C(int b, String h, String d) {
            super(b, h);
            this.dee = d;
        }
        public String toString() {
            return "C("+this.brandon+", "+this.hardy+","+this.dee+")";
        }
    }

    public static void main(String[] args) {
        System.out.println("Howdy");
        A a = new A(32, "hello");
        B b = new B(47, "goodbye", 1);
        C c = new C(47, "goodbye", "saturday");

        a = b;
        System.out.println(a);
        // b = a;
        // System.out.println(b);
        a = (A) b;
        System.out.println(a);
        b = (B) a;
        System.out.println(b);
        // b = (A) b;
        // System.out.println(b);
        // b = c;
        // System.out.println(b);
        // b = (A) c;
        // System.out.println(b);
        // b = (B) c;
        // System.out.println(b);
        // c = (C) a;
        // System.out.println(b);
    }
}
