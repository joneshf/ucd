/* *** This file is given as part of the programming assignment. *** */

public class Main {

    public static void main(String [] args) {

        // instantiate some sequences
 Constant c1 = new Constant(4, 11);
 Constant c2 = new Constant(4, 0);
 Constant c3 = new Constant(0, 11);
 Constant c4 = new Constant(0, 0);
 Constant c5 = new Constant(4, 88);
 Constant c6 = new Constant(3, 55);
 Constant c7 = new Constant(0, -77);
 Constant c8 = new Constant(10, 22);

 Delta d1 = new Delta(4, 1, 1);
 Delta d2 = new Delta(4, 1, -1);
 Delta d3 = new Delta(0, 1, -1);
 Delta d4 = new Delta(0, 1, 0);
 Delta d5 = new Delta(4, 1, 0);
 Delta d6 = new Delta(3, 21, 1);
 Delta d7 = new Delta(0, 25, -2);
 Delta d8 = new Delta(10, 2, 4);

 Jumble j1 = new Jumble(new int [] {});
 Jumble j2 = new Jumble(new int [] {88});
 Jumble j3 = new Jumble(new int [] {97, 55});
 Jumble j4 = new Jumble(new int []
 {199, 198, 197, 196, 195, 194, 193, 192});
 Jumble j5 = new Jumble(new int [] {102, 103, 101, 107, 109});
 Jumble j6 = new Jumble(new int [] {41, 42, 43, 44});
 // do everything twice just to double check
 for( int i = 0; i < 2; i++) {
     
             System.out.print( "c1" + ":"); 
             System.out.print(c1); 
             System.out.println(":"); 
             System.out.println( "(Seq)c1 instanceof Constant" + ":" + ((Seq)c1 instanceof Constant) + ":"); 
             System.out.println( "(Seq)c1 instanceof Delta" + ":" + ((Seq)c1 instanceof Delta) + ":"); 
             System.out.println( "(Seq)c1 instanceof Jumble" + ":" + ((Seq)c1 instanceof Jumble) + ":"); 
             System.out.println( "c1.min()" + ":" + (c1.min()) + ":");
     
             System.out.print( "c2" + ":"); 
             System.out.print(c2); 
             System.out.println(":"); 
             System.out.println( "(Seq)c2 instanceof Constant" + ":" + ((Seq)c2 instanceof Constant) + ":"); 
             System.out.println( "(Seq)c2 instanceof Delta" + ":" + ((Seq)c2 instanceof Delta) + ":"); 
             System.out.println( "(Seq)c2 instanceof Jumble" + ":" + ((Seq)c2 instanceof Jumble) + ":"); 
             System.out.println( "c2.min()" + ":" + (c2.min()) + ":");
     
             System.out.print( "c3" + ":"); 
             System.out.print(c3); 
             System.out.println(":"); 
             System.out.println( "(Seq)c3 instanceof Constant" + ":" + ((Seq)c3 instanceof Constant) + ":"); 
             System.out.println( "(Seq)c3 instanceof Delta" + ":" + ((Seq)c3 instanceof Delta) + ":"); 
             System.out.println( "(Seq)c3 instanceof Jumble" + ":" + ((Seq)c3 instanceof Jumble) + ":"); 
             System.out.println( "c3.min()" + ":" + (c3.min()) + ":");
     
             System.out.print( "c4" + ":"); 
             System.out.print(c4); 
             System.out.println(":"); 
             System.out.println( "(Seq)c4 instanceof Constant" + ":" + ((Seq)c4 instanceof Constant) + ":"); 
             System.out.println( "(Seq)c4 instanceof Delta" + ":" + ((Seq)c4 instanceof Delta) + ":"); 
             System.out.println( "(Seq)c4 instanceof Jumble" + ":" + ((Seq)c4 instanceof Jumble) + ":"); 
             System.out.println( "c4.min()" + ":" + (c4.min()) + ":");
     
             System.out.print( "c5" + ":"); 
             System.out.print(c5); 
             System.out.println(":"); 
             System.out.println( "(Seq)c5 instanceof Constant" + ":" + ((Seq)c5 instanceof Constant) + ":"); 
             System.out.println( "(Seq)c5 instanceof Delta" + ":" + ((Seq)c5 instanceof Delta) + ":"); 
             System.out.println( "(Seq)c5 instanceof Jumble" + ":" + ((Seq)c5 instanceof Jumble) + ":"); 
             System.out.println( "c5.min()" + ":" + (c5.min()) + ":");
     
             System.out.print( "c6" + ":"); 
             System.out.print(c6); 
             System.out.println(":"); 
             System.out.println( "(Seq)c6 instanceof Constant" + ":" + ((Seq)c6 instanceof Constant) + ":"); 
             System.out.println( "(Seq)c6 instanceof Delta" + ":" + ((Seq)c6 instanceof Delta) + ":"); 
             System.out.println( "(Seq)c6 instanceof Jumble" + ":" + ((Seq)c6 instanceof Jumble) + ":"); 
             System.out.println( "c6.min()" + ":" + (c6.min()) + ":");
     
             System.out.print( "c7" + ":"); 
             System.out.print(c7); 
             System.out.println(":"); 
             System.out.println( "(Seq)c7 instanceof Constant" + ":" + ((Seq)c7 instanceof Constant) + ":"); 
             System.out.println( "(Seq)c7 instanceof Delta" + ":" + ((Seq)c7 instanceof Delta) + ":"); 
             System.out.println( "(Seq)c7 instanceof Jumble" + ":" + ((Seq)c7 instanceof Jumble) + ":"); 
             System.out.println( "c7.min()" + ":" + (c7.min()) + ":");
     
             System.out.print( "c8" + ":"); 
             System.out.print(c8); 
             System.out.println(":"); 
             System.out.println( "(Seq)c8 instanceof Constant" + ":" + ((Seq)c8 instanceof Constant) + ":"); 
             System.out.println( "(Seq)c8 instanceof Delta" + ":" + ((Seq)c8 instanceof Delta) + ":"); 
             System.out.println( "(Seq)c8 instanceof Jumble" + ":" + ((Seq)c8 instanceof Jumble) + ":"); 
             System.out.println( "c8.min()" + ":" + (c8.min()) + ":");
     
             System.out.print( "d1" + ":"); 
             System.out.print(d1); 
             System.out.println(":"); 
             System.out.println( "(Seq)d1 instanceof Constant" + ":" + ((Seq)d1 instanceof Constant) + ":"); 
             System.out.println( "(Seq)d1 instanceof Delta" + ":" + ((Seq)d1 instanceof Delta) + ":"); 
             System.out.println( "(Seq)d1 instanceof Jumble" + ":" + ((Seq)d1 instanceof Jumble) + ":"); 
             System.out.println( "d1.min()" + ":" + (d1.min()) + ":");
     
             System.out.print( "d2" + ":"); 
             System.out.print(d2); 
             System.out.println(":"); 
             System.out.println( "(Seq)d2 instanceof Constant" + ":" + ((Seq)d2 instanceof Constant) + ":"); 
             System.out.println( "(Seq)d2 instanceof Delta" + ":" + ((Seq)d2 instanceof Delta) + ":"); 
             System.out.println( "(Seq)d2 instanceof Jumble" + ":" + ((Seq)d2 instanceof Jumble) + ":"); 
             System.out.println( "d2.min()" + ":" + (d2.min()) + ":");
     
             System.out.print( "d3" + ":"); 
             System.out.print(d3); 
             System.out.println(":"); 
             System.out.println( "(Seq)d3 instanceof Constant" + ":" + ((Seq)d3 instanceof Constant) + ":"); 
             System.out.println( "(Seq)d3 instanceof Delta" + ":" + ((Seq)d3 instanceof Delta) + ":"); 
             System.out.println( "(Seq)d3 instanceof Jumble" + ":" + ((Seq)d3 instanceof Jumble) + ":"); 
             System.out.println( "d3.min()" + ":" + (d3.min()) + ":");
     
             System.out.print( "d4" + ":"); 
             System.out.print(d4); 
             System.out.println(":"); 
             System.out.println( "(Seq)d4 instanceof Constant" + ":" + ((Seq)d4 instanceof Constant) + ":"); 
             System.out.println( "(Seq)d4 instanceof Delta" + ":" + ((Seq)d4 instanceof Delta) + ":"); 
             System.out.println( "(Seq)d4 instanceof Jumble" + ":" + ((Seq)d4 instanceof Jumble) + ":"); 
             System.out.println( "d4.min()" + ":" + (d4.min()) + ":");
     
             System.out.print( "d5" + ":"); 
             System.out.print(d5); 
             System.out.println(":"); 
             System.out.println( "(Seq)d5 instanceof Constant" + ":" + ((Seq)d5 instanceof Constant) + ":"); 
             System.out.println( "(Seq)d5 instanceof Delta" + ":" + ((Seq)d5 instanceof Delta) + ":"); 
             System.out.println( "(Seq)d5 instanceof Jumble" + ":" + ((Seq)d5 instanceof Jumble) + ":"); 
             System.out.println( "d5.min()" + ":" + (d5.min()) + ":");
     
             System.out.print( "d6" + ":"); 
             System.out.print(d6); 
             System.out.println(":"); 
             System.out.println( "(Seq)d6 instanceof Constant" + ":" + ((Seq)d6 instanceof Constant) + ":"); 
             System.out.println( "(Seq)d6 instanceof Delta" + ":" + ((Seq)d6 instanceof Delta) + ":"); 
             System.out.println( "(Seq)d6 instanceof Jumble" + ":" + ((Seq)d6 instanceof Jumble) + ":"); 
             System.out.println( "d6.min()" + ":" + (d6.min()) + ":");
     
             System.out.print( "d6" + ":"); 
             System.out.print(d6); 
             System.out.println(":"); 
             System.out.println( "(Seq)d6 instanceof Constant" + ":" + ((Seq)d6 instanceof Constant) + ":"); 
             System.out.println( "(Seq)d6 instanceof Delta" + ":" + ((Seq)d6 instanceof Delta) + ":"); 
             System.out.println( "(Seq)d6 instanceof Jumble" + ":" + ((Seq)d6 instanceof Jumble) + ":"); 
             System.out.println( "d6.min()" + ":" + (d6.min()) + ":");
     
             System.out.print( "d7" + ":"); 
             System.out.print(d7); 
             System.out.println(":"); 
             System.out.println( "(Seq)d7 instanceof Constant" + ":" + ((Seq)d7 instanceof Constant) + ":"); 
             System.out.println( "(Seq)d7 instanceof Delta" + ":" + ((Seq)d7 instanceof Delta) + ":"); 
             System.out.println( "(Seq)d7 instanceof Jumble" + ":" + ((Seq)d7 instanceof Jumble) + ":"); 
             System.out.println( "d7.min()" + ":" + (d7.min()) + ":");
     
             System.out.print( "d8" + ":"); 
             System.out.print(d8); 
             System.out.println(":"); 
             System.out.println( "(Seq)d8 instanceof Constant" + ":" + ((Seq)d8 instanceof Constant) + ":"); 
             System.out.println( "(Seq)d8 instanceof Delta" + ":" + ((Seq)d8 instanceof Delta) + ":"); 
             System.out.println( "(Seq)d8 instanceof Jumble" + ":" + ((Seq)d8 instanceof Jumble) + ":"); 
             System.out.println( "d8.min()" + ":" + (d8.min()) + ":");
     
             System.out.print( "j1" + ":"); 
             System.out.print(j1); 
             System.out.println(":"); 
             System.out.println( "(Seq)j1 instanceof Constant" + ":" + ((Seq)j1 instanceof Constant) + ":"); 
             System.out.println( "(Seq)j1 instanceof Delta" + ":" + ((Seq)j1 instanceof Delta) + ":"); 
             System.out.println( "(Seq)j1 instanceof Jumble" + ":" + ((Seq)j1 instanceof Jumble) + ":"); 
             System.out.println( "j1.min()" + ":" + (j1.min()) + ":");
     
             System.out.print( "j2" + ":"); 
             System.out.print(j2); 
             System.out.println(":"); 
             System.out.println( "(Seq)j2 instanceof Constant" + ":" + ((Seq)j2 instanceof Constant) + ":"); 
             System.out.println( "(Seq)j2 instanceof Delta" + ":" + ((Seq)j2 instanceof Delta) + ":"); 
             System.out.println( "(Seq)j2 instanceof Jumble" + ":" + ((Seq)j2 instanceof Jumble) + ":"); 
             System.out.println( "j2.min()" + ":" + (j2.min()) + ":");
     
             System.out.print( "j3" + ":"); 
             System.out.print(j3); 
             System.out.println(":"); 
             System.out.println( "(Seq)j3 instanceof Constant" + ":" + ((Seq)j3 instanceof Constant) + ":"); 
             System.out.println( "(Seq)j3 instanceof Delta" + ":" + ((Seq)j3 instanceof Delta) + ":"); 
             System.out.println( "(Seq)j3 instanceof Jumble" + ":" + ((Seq)j3 instanceof Jumble) + ":"); 
             System.out.println( "j3.min()" + ":" + (j3.min()) + ":");
     
             System.out.print( "j4" + ":"); 
             System.out.print(j4); 
             System.out.println(":"); 
             System.out.println( "(Seq)j4 instanceof Constant" + ":" + ((Seq)j4 instanceof Constant) + ":"); 
             System.out.println( "(Seq)j4 instanceof Delta" + ":" + ((Seq)j4 instanceof Delta) + ":"); 
             System.out.println( "(Seq)j4 instanceof Jumble" + ":" + ((Seq)j4 instanceof Jumble) + ":"); 
             System.out.println( "j4.min()" + ":" + (j4.min()) + ":");
     
             System.out.print( "j5" + ":"); 
             System.out.print(j5); 
             System.out.println(":"); 
             System.out.println( "(Seq)j5 instanceof Constant" + ":" + ((Seq)j5 instanceof Constant) + ":"); 
             System.out.println( "(Seq)j5 instanceof Delta" + ":" + ((Seq)j5 instanceof Delta) + ":"); 
             System.out.println( "(Seq)j5 instanceof Jumble" + ":" + ((Seq)j5 instanceof Jumble) + ":"); 
             System.out.println( "j5.min()" + ":" + (j5.min()) + ":");
     
             System.out.print( "j6" + ":"); 
             System.out.print(j6); 
             System.out.println(":"); 
             System.out.println( "(Seq)j6 instanceof Constant" + ":" + ((Seq)j6 instanceof Constant) + ":"); 
             System.out.println( "(Seq)j6 instanceof Delta" + ":" + ((Seq)j6 instanceof Delta) + ":"); 
             System.out.println( "(Seq)j6 instanceof Jumble" + ":" + ((Seq)j6 instanceof Jumble) + ":"); 
             System.out.println( "j6.min()" + ":" + (j6.min()) + ":");
     //check a bit more
     Seq x;
     x = j3;
     
             System.out.print( "x" + ":"); 
             System.out.print(x); 
             System.out.println(":"); 
             System.out.println( "(Seq)x instanceof Constant" + ":" + ((Seq)x instanceof Constant) + ":"); 
             System.out.println( "(Seq)x instanceof Delta" + ":" + ((Seq)x instanceof Delta) + ":"); 
             System.out.println( "(Seq)x instanceof Jumble" + ":" + ((Seq)x instanceof Jumble) + ":"); 
             System.out.println( "x.min()" + ":" + (x.min()) + ":");
     x = d4;
     
             System.out.print( "x" + ":"); 
             System.out.print(x); 
             System.out.println(":"); 
             System.out.println( "(Seq)x instanceof Constant" + ":" + ((Seq)x instanceof Constant) + ":"); 
             System.out.println( "(Seq)x instanceof Delta" + ":" + ((Seq)x instanceof Delta) + ":"); 
             System.out.println( "(Seq)x instanceof Jumble" + ":" + ((Seq)x instanceof Jumble) + ":"); 
             System.out.println( "x.min()" + ":" + (x.min()) + ":");
     x = c1;
     
             System.out.print( "x" + ":"); 
             System.out.print(x); 
             System.out.println(":"); 
             System.out.println( "(Seq)x instanceof Constant" + ":" + ((Seq)x instanceof Constant) + ":"); 
             System.out.println( "(Seq)x instanceof Delta" + ":" + ((Seq)x instanceof Delta) + ":"); 
             System.out.println( "(Seq)x instanceof Jumble" + ":" + ((Seq)x instanceof Jumble) + ":"); 
             System.out.println( "x.min()" + ":" + (x.min()) + ":");
 }
 // okay, if you're still not convinced ...
 Seq b[] = new Seq [8];
 b[0] = new Constant(8, 888);
 b[1] = new Jumble(new int [] {77, 78, 79});
 b[2] = new Jumble(new int [] {81, 82});
 b[3] = new Delta(4, 5, 1);
 b[4] = new Jumble(new int [] {});
 b[5] = new Jumble(new int [] {10, 1, 2, 3, 4, 5, 10, -1, -2, -3, -4});
 b[6] = new Delta(11, 0, 5);
 b[7] = new Constant(22, 222);
 for (int k = 0; k < 16; k++) {
     
             System.out.print( "b[k%8]" + ":"); 
             System.out.print(b[k%8]); 
             System.out.println(":"); 
             System.out.println( "(Seq)b[k%8] instanceof Constant" + ":" + ((Seq)b[k%8] instanceof Constant) + ":"); 
             System.out.println( "(Seq)b[k%8] instanceof Delta" + ":" + ((Seq)b[k%8] instanceof Delta) + ":"); 
             System.out.println( "(Seq)b[k%8] instanceof Jumble" + ":" + ((Seq)b[k%8] instanceof Jumble) + ":"); 
             System.out.println( "b[k%8].min()" + ":" + (b[k%8].min()) + ":");
 }
 // did you copy Jumble's array?
 int [] aa = {8, 4, 11};
 Jumble jj1 = new Jumble(aa);
 
             System.out.print( "jj1" + ":"); 
             System.out.print(jj1); 
             System.out.println(":"); 
             System.out.println( "(Seq)jj1 instanceof Constant" + ":" + ((Seq)jj1 instanceof Constant) + ":"); 
             System.out.println( "(Seq)jj1 instanceof Delta" + ":" + ((Seq)jj1 instanceof Delta) + ":"); 
             System.out.println( "(Seq)jj1 instanceof Jumble" + ":" + ((Seq)jj1 instanceof Jumble) + ":"); 
             System.out.println( "jj1.min()" + ":" + (jj1.min()) + ":");
 aa[1] = 9999;
 Jumble jj2 = new Jumble(aa);
 
             System.out.print( "jj2" + ":"); 
             System.out.print(jj2); 
             System.out.println(":"); 
             System.out.println( "(Seq)jj2 instanceof Constant" + ":" + ((Seq)jj2 instanceof Constant) + ":"); 
             System.out.println( "(Seq)jj2 instanceof Delta" + ":" + ((Seq)jj2 instanceof Delta) + ":"); 
             System.out.println( "(Seq)jj2 instanceof Jumble" + ":" + ((Seq)jj2 instanceof Jumble) + ":"); 
             System.out.println( "jj2.min()" + ":" + (jj2.min()) + ":");
 // jj1 shouldn't have been changed
 
             System.out.print( "jj1" + ":"); 
             System.out.print(jj1); 
             System.out.println(":"); 
             System.out.println( "(Seq)jj1 instanceof Constant" + ":" + ((Seq)jj1 instanceof Constant) + ":"); 
             System.out.println( "(Seq)jj1 instanceof Delta" + ":" + ((Seq)jj1 instanceof Delta) + ":"); 
             System.out.println( "(Seq)jj1 instanceof Jumble" + ":" + ((Seq)jj1 instanceof Jumble) + ":"); 
             System.out.println( "jj1.min()" + ":" + (jj1.min()) + ":");
 System.exit(0);
    }
}
