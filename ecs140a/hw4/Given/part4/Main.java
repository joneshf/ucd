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
 // iterators
 System.out.print( "myprintc(c1)" + ":"); 
             myprintc(c1); 
             System.out.println(":");
 System.out.print( "myprintc(c2)" + ":"); 
             myprintc(c2); 
             System.out.println(":");
 System.out.print( "myprintc(c3)" + ":"); 
             myprintc(c3); 
             System.out.println(":");
 System.out.print( "myprintc(c4)" + ":"); 
             myprintc(c4); 
             System.out.println(":");
 System.out.print( "myprintc(c5)" + ":"); 
             myprintc(c5); 
             System.out.println(":");
 System.out.print( "myprintc(c6)" + ":"); 
             myprintc(c6); 
             System.out.println(":");
 System.out.print( "myprintc(c7)" + ":"); 
             myprintc(c7); 
             System.out.println(":");
 System.out.print( "myprintc(c8)" + ":"); 
             myprintc(c8); 
             System.out.println(":");
 System.out.print( "myprintd(d1)" + ":"); 
             myprintd(d1); 
             System.out.println(":");
 System.out.print( "myprintd(d2)" + ":"); 
             myprintd(d2); 
             System.out.println(":");
 System.out.print( "myprintd(d3)" + ":"); 
             myprintd(d3); 
             System.out.println(":");
 System.out.print( "myprintd(d4)" + ":"); 
             myprintd(d4); 
             System.out.println(":");
 System.out.print( "myprintd(d5)" + ":"); 
             myprintd(d5); 
             System.out.println(":");
 System.out.print( "myprintd(d6)" + ":"); 
             myprintd(d6); 
             System.out.println(":");
 System.out.print( "myprintd(d7)" + ":"); 
             myprintd(d7); 
             System.out.println(":");
 System.out.print( "myprintd(d8)" + ":"); 
             myprintd(d8); 
             System.out.println(":");
 System.out.print( "myprintj(j1)" + ":"); 
             myprintj(j1); 
             System.out.println(":");
 System.out.print( "myprintj(j2)" + ":"); 
             myprintj(j2); 
             System.out.println(":");
 System.out.print( "myprintj(j3)" + ":"); 
             myprintj(j3); 
             System.out.println(":");
 System.out.print( "myprintj(j4)" + ":"); 
             myprintj(j4); 
             System.out.println(":");
 System.out.print( "myprintj(j5)" + ":"); 
             myprintj(j5); 
             System.out.println(":");
 System.out.print( "myprintj(j6)" + ":"); 
             myprintj(j6); 
             System.out.println(":");
 System.out.print( "mycrossc(c1,c3)" + ":"); 
             mycrossc(c1,c3); 
             System.out.println(":");
 System.out.print( "mycrossc(c1,c1)" + ":"); 
             mycrossc(c1,c1); 
             System.out.println(":");
 System.out.print( "mycrossc(c3,c3)" + ":"); 
             mycrossc(c3,c3); 
             System.out.println(":");
 System.out.print( "mycrossc(c6,c6)" + ":"); 
             mycrossc(c6,c6); 
             System.out.println(":");
 System.out.print( "mycrossc(c1,c2)" + ":"); 
             mycrossc(c1,c2); 
             System.out.println(":");
 System.out.print( "mycrossc(c2,c2)" + ":"); 
             mycrossc(c2,c2); 
             System.out.println(":");
 System.out.print( "mycrossc(c7,c4)" + ":"); 
             mycrossc(c7,c4); 
             System.out.println(":");
 System.out.print( "mycrossd(d1,d3)" + ":"); 
             mycrossd(d1,d3); 
             System.out.println(":");
 System.out.print( "mycrossd(d3,d1)" + ":"); 
             mycrossd(d3,d1); 
             System.out.println(":");
 System.out.print( "mycrossd(d1,d1)" + ":"); 
             mycrossd(d1,d1); 
             System.out.println(":");
 System.out.print( "mycrossd(d8,d1)" + ":"); 
             mycrossd(d8,d1); 
             System.out.println(":");
 System.out.print( "mycrossd(d1,d8)" + ":"); 
             mycrossd(d1,d8); 
             System.out.println(":");
 System.out.print( "mycrossd(d2,d3)" + ":"); 
             mycrossd(d2,d3); 
             System.out.println(":");
 System.out.print( "mycrossd(d3,d2)" + ":"); 
             mycrossd(d3,d2); 
             System.out.println(":");
 System.out.print( "mycrossd(d1,d5)" + ":"); 
             mycrossd(d1,d5); 
             System.out.println(":");
 System.out.print( "mycrossd(d4,d5)" + ":"); 
             mycrossd(d4,d5); 
             System.out.println(":");
 System.out.print( "mycrossj(j1,j3)" + ":"); 
             mycrossj(j1,j3); 
             System.out.println(":");
 System.out.print( "mycrossj(j3,j1)" + ":"); 
             mycrossj(j3,j1); 
             System.out.println(":");
 System.out.print( "mycrossj(j1,j1)" + ":"); 
             mycrossj(j1,j1); 
             System.out.println(":");
 System.out.print( "mycrossj(j6,j1)" + ":"); 
             mycrossj(j6,j1); 
             System.out.println(":");
 System.out.print( "mycrossj(j1,j6)" + ":"); 
             mycrossj(j1,j6); 
             System.out.println(":");
 System.out.print( "mycrossj(j2,j3)" + ":"); 
             mycrossj(j2,j3); 
             System.out.println(":");
 System.out.print( "mycrossj(j3,j2)" + ":"); 
             mycrossj(j3,j2); 
             System.out.println(":");
 System.out.print( "mycrossj(j1,j5)" + ":"); 
             mycrossj(j1,j5); 
             System.out.println(":");
 System.out.print( "mycrossj(j4,j5)" + ":"); 
             mycrossj(j4,j5); 
             System.out.println(":");
 // test virtualness
 try {
     SeqIt si = new DeltaIt(d1);
     System.out.println( "si.next()" + ":" + (si.next()) + ":");
     System.out.println( "si.next()" + ":" + (si.next()) + ":");
     si = new ConstantIt(c1);
     System.out.println( "si.next()" + ":" + (si.next()) + ":");
     System.out.println( "si.next()" + ":" + (si.next()) + ":");
     si = new JumbleIt(j4);
     System.out.println( "si.next()" + ":" + (si.next()) + ":");
     System.out.println( "si.next()" + ":" + (si.next()) + ":");
 }
        catch (UsingIteratorPastEndException e) {
            System.out.println("oops! caught UsingIteratorPastEndException from si.next()");
        }
 try {
     System.out.println("test of calling ConstantIt"
               +" too many times");
         ConstantIt ci = new ConstantIt(new Constant(3, 55));
     System.out.println( "ci.next()" + ":" + (ci.next()) + ":");
     System.out.println( "ci.next()" + ":" + (ci.next()) + ":");
     System.out.println( "ci.next()" + ":" + (ci.next()) + ":");
     System.out.println( "ci.next()" + ":" + (ci.next()) + ":");
     System.out.println( "ci.next()" + ":" + (ci.next()) + ":");
 }
        catch (UsingIteratorPastEndException e) {
            System.out.println("= caught UsingIteratorPastEndException from ConstantIt");
        }
        try {
     System.out.println("test of calling DeltaIt"
               +" too many times");
       DeltaIt di = new DeltaIt(new Delta(4, 5, 9));
     System.out.println( "di.next()" + ":" + (di.next()) + ":");
     System.out.println( "di.next()" + ":" + (di.next()) + ":");
     System.out.println( "di.next()" + ":" + (di.next()) + ":");
     System.out.println( "di.next()" + ":" + (di.next()) + ":");
     System.out.println( "di.next()" + ":" + (di.next()) + ":");
 }
        catch (UsingIteratorPastEndException e) {
            System.out.println("= caught UsingIteratorPastEndException from DeltaIt");
        }
        try {
     System.out.println("test of calling JumbleIt"
               +" too many times");
         JumbleIt ji = new JumbleIt(new Jumble( new int [] {76, 77}));
     System.out.println( "ji.next()" + ":" + (ji.next()) + ":");
     System.out.println( "ji.next()" + ":" + (ji.next()) + ":");
     System.out.println( "ji.next()" + ":" + (ji.next()) + ":");
     System.out.println( "ji.next()" + ":" + (ji.next()) + ":");
     System.out.println( "ji.next()" + ":" + (ji.next()) + ":");
 }
        catch (UsingIteratorPastEndException e) {
            System.out.println("= caught UsingIteratorPastEndException from JumbleIt");
        }
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j1)" + ":" + (JumbleUser.lengthLongestNDCSS1(j1)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j2)" + ":" + (JumbleUser.lengthLongestNDCSS1(j2)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j3)" + ":" + (JumbleUser.lengthLongestNDCSS1(j3)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j4)" + ":" + (JumbleUser.lengthLongestNDCSS1(j4)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j5)" + ":" + (JumbleUser.lengthLongestNDCSS1(j5)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j6)" + ":" + (JumbleUser.lengthLongestNDCSS1(j6)) + ":");
 Jumble j7 = new Jumble(new int []
 {5, 6, 7, 1, 2, 2, 3, 2, 4, 5, 6, 9, 0, 0, 3});
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j7)" + ":" + (JumbleUser.lengthLongestNDCSS1(j7)) + ":");
 Jumble j8 = new Jumble(new int []
 {5, 6, 7, 8, 8, 9, 2, 4, 5, 6, 1, 0, 0, 3});
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j8)" + ":" + (JumbleUser.lengthLongestNDCSS1(j8)) + ":");
 Jumble j9 = new Jumble(new int []
 {5, 6, 5, 8, 8, 8, 5, 4, 5, 6, 1, 0, 0, 3});
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j9)" + ":" + (JumbleUser.lengthLongestNDCSS1(j9)) + ":");
 Jumble j10 = new Jumble(new int []
 {5, 6, 5, 8, 8, 8, 5, 4, 5, 6, 1, 0, 0, 3});
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j10)" + ":" + (JumbleUser.lengthLongestNDCSS1(j10)) + ":");
 Jumble j11 = new Jumble(new int []
 {5, 6, 5, 6, 5, 6, 5, 6, 5, 6, 5, 0, 0, 0});
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j11)" + ":" + (JumbleUser.lengthLongestNDCSS1(j11)) + ":");
 Jumble j12 = new Jumble(new int []
 {-5, -6, -5, -6, -5, -6, -5, -6, -5, -6, -5, 0, 0, 0});
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j12)" + ":" + (JumbleUser.lengthLongestNDCSS1(j12)) + ":");
 Jumble j13 = new Jumble(new int []
 {-5, -6, -5, -6, -5, -6, -5, -6, -5, -6, -5});
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j13)" + ":" + (JumbleUser.lengthLongestNDCSS1(j13)) + ":");
 Jumble j14 = new Jumble(new int []
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0});
 System.out.println( "JumbleUser.lengthLongestNDCSS1(j14)" + ":" + (JumbleUser.lengthLongestNDCSS1(j14)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j1)" + ":" + (JumbleUser.lengthLongestNDCSS2(j1)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j2)" + ":" + (JumbleUser.lengthLongestNDCSS2(j2)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j3)" + ":" + (JumbleUser.lengthLongestNDCSS2(j3)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j4)" + ":" + (JumbleUser.lengthLongestNDCSS2(j4)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j5)" + ":" + (JumbleUser.lengthLongestNDCSS2(j5)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j6)" + ":" + (JumbleUser.lengthLongestNDCSS2(j6)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j7)" + ":" + (JumbleUser.lengthLongestNDCSS2(j7)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j8)" + ":" + (JumbleUser.lengthLongestNDCSS2(j8)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j9)" + ":" + (JumbleUser.lengthLongestNDCSS2(j9)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j10)" + ":" + (JumbleUser.lengthLongestNDCSS2(j10)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j11)" + ":" + (JumbleUser.lengthLongestNDCSS2(j11)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j12)" + ":" + (JumbleUser.lengthLongestNDCSS2(j12)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j13)" + ":" + (JumbleUser.lengthLongestNDCSS2(j13)) + ":");
 System.out.println( "JumbleUser.lengthLongestNDCSS2(j14)" + ":" + (JumbleUser.lengthLongestNDCSS2(j14)) + ":");
 System.exit(0);
    }
    private static void myprintc(Constant c) {
 ConstantIt ci = new ConstantIt(c);
        while( ci.hasNext() ) {
     try {
         System.out.print(ci.next() + " ");
     }
            catch (UsingIteratorPastEndException e) {
                System.out.println("myprintc oops! caught UsingIteratorPastEndException");
            }
 }
    }
    private static void myprintd(Delta d) {
 DeltaIt di = new DeltaIt(d);
        while( di.hasNext() ) {
     try {
         System.out.print(di.next() + " ");
     }
            catch (UsingIteratorPastEndException e) {
                System.out.println("myprintd oops! caught UsingIteratorPastEndException");
            }
 }
    }
    private static void myprintj(Jumble d) {
 JumbleIt ji = new JumbleIt(d);
        while( ji.hasNext() ) {
     try {
         System.out.print(ji.next() + " ");
     }
            catch (UsingIteratorPastEndException e) {
                System.out.println("myprintj oops! caught UsingIteratorPastEndException");
            }
 }
    }
    private static void mycrossc(Constant c1, Constant c2) {
 ConstantIt ci1 = new ConstantIt(c1);
 boolean first = true;
 while ( ci1.hasNext() ) {
     try {
         int p1 = ci1.next();
                ConstantIt ci2 = new ConstantIt(c2);
         while ( ci2.hasNext() ) {
      int p2 = ci2.next();
      if ( first ) {
          first = false;
          System.out.println();
      }
      System.out.println("   " + p1 + " " + p2);
         }
     }
            catch (UsingIteratorPastEndException e) {
                System.out.println("mycrossc oops! caught UsingIteratorPastEndException");
            }
 }
    }
    private static void mycrossd(Delta d1, Delta d2) {
 DeltaIt di1 = new DeltaIt(d1);
 boolean first = true;
 while ( di1.hasNext() ) {
     try {
         int p1 = di1.next();
                DeltaIt di2 = new DeltaIt(d2);
         while ( di2.hasNext() ) {
      int p2 = di2.next();
      if ( first ) {
          first = false;
          System.out.println();
      }
      System.out.println("   " + p1 + " " + p2);
         }
     }
            catch (UsingIteratorPastEndException e) {
                System.out.println("mycrossd oops! caught UsingIteratorPastEndException");
     }
 }
    }
    private static void mycrossj(Jumble j1, Jumble j2) {
 JumbleIt ji1 = new JumbleIt(j1);
 boolean first = true;
 while ( ji1.hasNext() ) {
     try {
         int p1 = ji1.next();
                JumbleIt ji2 = new JumbleIt(j2);
         while ( ji2.hasNext() ) {
      int p2 = ji2.next();
      if ( first ) {
          first = false;
          System.out.println();
      }
      System.out.println("   " + p1 + " " + p2);
         }
     }
            catch (UsingIteratorPastEndException e) {
                System.out.println("mycrossj oops! caught UsingIteratorPastEndException");
            }
 }
    }
}
