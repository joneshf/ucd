/* *** This file is given as part of the programming assignment. *** */

import java.io.*;

// Note this code is written to aid the student in learning Java.
// A Java expert could certainly improve the efficiency, conciseness, and
// style of this code.

public class Scan {

    // default max length of identifiers; truncate extra.
    public static final int MAXLEN_ID = 40;

    public Scan(String args[]) { // the constructor
        // open an input file if one specified on command line.
        // o.w., use standard input.
        try {
            if (args.length == 0) {
                InputStreamReader isr = new InputStreamReader(System.in);
                br = new BufferedReader(isr);
            }
            else if (args.length > 1) {
                System.err.println("too many command line arguments("+
                                   args.length+"); want 0 or 1" );
                System.exit(1);
            }
            else {
                FileReader fr = new FileReader(args[0]);
                br = new BufferedReader(fr);
            }
        }
        catch (Exception oops) {
            System.err.println("Exception opening file "+args[0]+"\n");
            oops.printStackTrace();
        }

        // initially, we pretend that we just read a newline.
        // hence linenumber is initialized to 0.
        c = '\n';
        linenumber = 0;
        putback = true;
        got_eof = false;
    }

    // internal state of scanner
    private BufferedReader br;  // input stream
    private String token;    // the token as a string
    private TK tkrep;        // the kind of token
    private int linenumber;  // line number

    private boolean got_eof; // true iff have seen EOF
    private boolean putback; // true iff put a char back
    private int c;           // current or putback char
                             // (int rather than char to handle EOF)
    private static final int EOF = -1;

    // call to advance token stream.
    // acts as a generator (iterator) over input.
    // returns Token
    public Token scan() {
        if( got_eof ) {
            System.err.println("scan: oops -- called after eof.");
            return new Token(TK.ERROR, "called after eof", linenumber);
        }

        while(true) {
// QUESTION 1: What is purpose of putback?

// QUESTION 2: What are the values of putback and c just before the identifier
//             'hello' is returned from the input 'hello*45'?

            if( putback) {
                putback = false;
            }
            else {
                c = getchar();
            }
            if ( myisalpha((char) c) ) {
                /* identifier. */
                String id = buildID();
                return new Token(keywordLookup(id), id, linenumber);
            }
            else if ( myisdigit((char) c) ) {
                /* number. */
                return new Token(TK.NUM, buildNUM(), linenumber);
            }
            else {
                switch( c ) {
                    case '(':
                        return ccase1('(',TK.LPAREN);
                    case ')':
                        return ccase1(')',TK.RPAREN);
                    case '*':
                        return ccase1('*',TK.TIMES);
                    case '=':
                        return ccase1('=',TK.EQ);

// QUESTION 3:  What does the following case and the code in it do?

                    case '/':
                        return ccase1or2('/','=',TK.DIVIDE,TK.NE);

                    case '<':
                        return ccase1or2('<','=',TK.LT,TK.LE);
                    case '>':
                        return ccase1or2('>','=',TK.GT,TK.GE);

                    case ':':
                        return ccase2(':','=',TK.ASSIGN);

                    case '[':
                        return ccase2('[',']',TK.BOX);
                    case '-':
                        return ccase1or2('-','>',TK.MINUS,TK.ARROW);

                    case EOF:
                        got_eof = true;
                        return new Token(TK.EOF,
                                         new String("*EOF*"),
                                         linenumber);
                    case '\n':
                        linenumber++;
                        break;
                    case ' ':
                    case '\t':
                    case '\r': // for Windows (lines end in \r\n)
                        break; // whitespace is easy to ignore
                    case '#': // gobble comments
                        do {
                            c = getchar();
                        } while( c != '\n' && c != EOF );
                        putback = true;
                        break;
                    default:
                        System.err.println(
                                           "scan: line "+linenumber+
                                           " bad char (ASCII " + c
                                           + ")");
                        break;
                }
            }
        }
    }


    private int getchar() {
        int c = EOF;
        try {
            c = br.read();
        } catch (java.io.IOException e) {
            System.err.println("oops ");
            e.printStackTrace();
        }
        return c;
    }

    private Token ccase1(char c, TK r) {
        return new Token(r, new String(String.valueOf(c)), linenumber);
    }

    private Token ccase1or2(char c1, char c2, TK r1, TK r2) {
        c = getchar();
        if (c == c2) {
            return new Token(
                    r2,
                    new String(String.valueOf(c1)+String.valueOf(c2)),
                    linenumber);
        }
        else {
            putback = true;
            return new Token(r1, new String(String.valueOf(c1)), linenumber);
        }
    }

    private Token ccase2(char c1, char c2, TK r) {
        c = getchar();
        if (c == c2) {
            return new Token(
                     r, String.valueOf(c1)+String.valueOf(c2),
                     linenumber);
        }
        else {
            System.err.println("scan: got " + c1 +
                               " missing " + c2 +
                               " (got ASCII " + c + ")");
            return new Token(TK.ERROR, "bad ccase2", linenumber);
        }
    }

    // rather than duplicating code, as done below,
    // could use method "pointer" technique for these build methods.

    // build up an ID token
    // (could use StringBuffer to make this more efficient...)
    private String buildID() {
        int k = 0;
        String str = "";
        do {
            str += (char) c;
            k++;
            c = getchar();
        } while( myisalpha((char) c) && k < MAXLEN_ID );
        putback = true;
        if( myisalpha((char) c) && k == MAXLEN_ID ) {
            do { c = getchar(); } while(myisalpha((char) c));
            System.err.println("scan: identifier too long -- truncated to "
                               + str);
        }
        return str;
    }

    // build up a NUM str
    // (could use StringBuffer to make this more efficient...)
    private String buildNUM() {
        int k = 0;
        String str = "";
        do {
            str += (char) c;
            k++;
            c = getchar();
        } while( myisdigit((char) c) && k < MAXLEN_ID );
        putback = true;
        if( myisdigit((char) c) && k == MAXLEN_ID ) {
            do { c = getchar(); } while(myisdigit((char) c));
            System.err.println("scan: number too long -- truncated to "
                               + str);
        }
        return str;
    }


    // E's idea of what can form an identifier
    // (could instead directly call Character.isLetter)
    private static boolean myisalpha(char c) {
        return Character.isLetter(c);
    }

    // E's idea of what can form a number
    // (could instead directly call Character.isDigit)
    private static boolean myisdigit(char c) {
        return Character.isDigit(c);
    }

    // if str is a keyword, return its TK.
    // otherwise, str is an identifier, so return TK.ID.
    private TK keywordLookup(String str) {
        // test for each keyword token, one after another.
        //(not best way to handle this, but expedient.)
        if (str.equals("var"))       return TK.VAR;
        if (str.equals("rav"))       return TK.RAV;
        if (str.equals("print"))     return TK.PRINT;
        if (str.equals("if"))        return TK.IF;
        if (str.equals("fi"))        return TK.FI;
        if (str.equals("do"))        return TK.DO;
        if (str.equals("od"))        return TK.OD;
        if (str.equals("else"))      return TK.ELSE;
        if (str.equals("fa"))        return TK.FA;
        if (str.equals("af"))        return TK.AF;
        if (str.equals("to"))        return TK.TO;
        if (str.equals("st"))        return TK.ST;
 
        // no keyword matched, so ...
        return TK.ID;
    }

}
