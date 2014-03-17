public class Midterm2_F2013 {
    public static void main(String[] args) {
        A a = new A();
        B b = new B();
        C c = new C();
        A a2 = new A();
        A.f();
        B.f();
        C.f();
        a.f();
        b.f();
        c.f();
        System.out.println("middle");
        a = (A) b;
        a = (B) a2;
        if (a instanceof A) {
            System.out.println("a is an A");
        } else if (a instanceof B) {
            System.out.println("a is a B");
        } else {
            System.out.println("a is a C");
        }
        System.out.println("a's class is: " + a.getClass());
        a = b;
        if (a instanceof A) {
            System.out.println("a is an A");
        } else if (a instanceof B) {
            System.out.println("a is a B");
        } else {
            System.out.println("a is a C");
        }
        System.out.println("a's class is: " + a.getClass());
        a.f();
        a = c;
        if (a instanceof A) {
            System.out.println("a is an A");
        } else if (a instanceof B) {
            System.out.println("a is a B");
        } else {
            System.out.println("a is a C");
        }
        System.out.println("a's class is: " + a.getClass());
        a.f();
        (new B()).f();
        a = null;
        a.f();
    }

    public static class A {
        public static void f() {
            System.out.println("A");
        }
    }

    public static class B extends A {
        public static void f() {
            System.out.println("B");
        }
    }

    public static class C extends A {}
}
