
public class Unary extends CG {

    public Unary(Scan scanner) {
        super(scanner);
    }

    protected void cIncludes() {
        super.cIncludes();
        cLine("#include <math.h>");
    }

    protected void cSquarePrelude() {
        cLine("int mySquare(int n) {");
        ++indent;
        cLine("return n * n;");
        --indent;
        cLine("}");
        cLine();
    }

    protected void cSqrtPrelude() {
        cLine("int mySqrt(int n) {");
        ++indent;
        cLine("if (n < 0) {");
        ++indent;
        cLine("return 0;");
        --indent;
        cLine("} else {");
        ++indent;
        cLine("return (int) sqrt(n);");
        --indent;
        cLine("}");
        --indent;
        cLine("}");
        cLine();
    }

    protected void cPrelude() {
        super.cPrelude();
        cSquarePrelude();
        cSqrtPrelude();
    }

    protected void parseFactor() {
        if (isParenthesized()) {
            parseParenthesized();
        } else if (isId()) {
            Token t = new Token(tok.kind, tok.string, tok.lineNumber);
            mustbe(TK.ID);
            addUsed(t);
        } else if (isNumber()) {
            parseNumber();
        } else if (isSquare()) {
            parseSquare();
        } else if (isSqrt()) {
            parseSqrt();
        } else {
            parseError("factor");
        }
    }

    protected void parseSqrt() {
        mustbe(TK.SQRT);
        parseExpression();
        cStr(")");
    }

    protected void parseSquare() {
        mustbe(TK.SQUARE);
        parseExpression();
        cStr(")");
    }

    protected boolean isSqrt() {
        return is(TK.SQRT);
    }

    protected boolean isSquare() {
        return is(TK.SQUARE);
    }

    protected void mustbe(TK tk) {
        if (!is(tk)) {
            System.err.println("mustbe: want " + tk + ", got " + tok);
            parseError("missing token (mustbe)");
        }
        if (tk == TK.ID && isUndeclared()) {
            System.err.println(undeclared());
            System.exit(1);
        }
        // We can do everything from here.
        switch (tk) {
            case VAR:
                cStrIndent("int ");
                break;
            case RAV:
                cStr(";\n");
                break;
            case IF:
            case DO:
                cStrIndent("if (");
                break;
                // cStrIndent("while (");
                // break;
            case ELSE:
                cLine("} else {");
                break;
            case FA:
                cStrIndent("for (");
                break;
            case TO:
                cStr("; ");
                break;
            case ST:
                cStrIndent("if (");
                break;
            case PRINT:
                cStrIndent("printf(\"%d\\n\", ");
                break;
            case FI:
            case OD:
            case AF:
                cLine("}");
                break;
            case ARROW:
                cStr("");
                break;
            case BOX:
                cStrIndent("} else if (");
                break;
            case ASSIGN:
                cStr(" = ");
                break;
            case NE:
                cStr(" != ");
                break;
            case EQ:
                cStr(" == ");
                break;
            case PLUS:
            case MINUS:
            case TIMES:
            case DIVIDE:
            case LT:
            case GT:
            case LE:
            case GE:
                cStr(" "+tok.string+" ");
                break;
            case ID:
                cStr("x_"+tok.string);
                break;
            case SQRT:
                cStr("mySqrt(");
                break;
            case SQUARE:
                cStr("mySquare(");
                break;
            default:
                cStr(tok.string);
                break;
        }
        scan();
    }
}
