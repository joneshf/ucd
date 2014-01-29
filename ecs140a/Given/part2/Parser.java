/* *** This file is given as part of the programming assignment. *** */

public class Parser {

    // tok is global to all these parsing methods;
    // scan just calls the scanner's scan method and saves the result in tok.
    protected Token tok; // the current token
    protected void scan() {
        tok = scanner.scan();
    }

    protected Scan scanner;
    Parser(Scan scanner) {
        this.scanner = scanner;
        scan();
        parseProgram();
        if (tok.kind != TK.EOF)
            parseError("junk after logical end of program");
    }

    protected void parseProgram() {
        parseBlock();
    }

    protected void parseAddOp() {
        switch (tok.kind) {
            case PLUS:
                parsePlus();
                break;
            case MINUS:
                parseMinus();
                break;
            default:
                parseError("addop");
                break;
        }
    }

    protected void parseAssignment() {
        mustbe(TK.ID);
        mustbe(TK.ASSIGN);
        parseExpression();
    }

    protected void parseBlock() {
        // Optional declarations.
        if (isDeclaration()) {
            parseDeclarations();
        }
        parseStatementList();
    }

    protected void parseBox() {
        mustbe(TK.BOX);
    }

    protected void parseCommands() {
        mustbe(TK.ARROW);
        parseBlock();
    }

    protected void parseDeclarations() {
        mustbe(TK.VAR);
        while (is(TK.ID)) {
            scan();
        }
        mustbe(TK.RAV);
    }

    protected void parseDivide() {
        mustbe(TK.DIVIDE);
    }

    protected void parseDo() {
        mustbe(TK.DO);
        parseGuardedCommands();
        mustbe(TK.OD);
    }

    protected void parseElse() {
        mustbe(TK.ELSE);
    }

    protected void parseEq() {
        mustbe(TK.EQ);
    }

    protected void parseExpression() {
        parseSimple();
        if (isRelOp()) {
            parseRelOp();
            parseSimple();
        }
    }

    protected void parseFa() {
        mustbe(TK.FA);
        mustbe(TK.ID);
        mustbe(TK.ASSIGN);
        parseExpression();
        mustbe(TK.TO);
        parseExpression();
        if (isSt()) {
            parseSt();
            parseExpression();
        }
        parseCommands();
        mustbe(TK.AF);
    }

    protected void parseFactor() {
        if (isParenthesized()) {
            parseParenthesized();
        } else if (isId()) {
            parseId();
        } else if (isNumber()) {
            parseNumber();
        } else {
            parseError("factor");
        }
    }

    protected void parseGe() {
        mustbe(TK.GE);
    }

    protected void parseGt() {
        mustbe(TK.GT);
    }

    protected void parseGuardedCommand() {
        parseExpression();
        parseCommands();
    }

    protected void parseGuardedCommands() {
        parseGuardedCommand();
        while (isBox()) {
            parseBox();
            parseGuardedCommand();
        }
        if (isElse()) {
            parseElse();
            parseCommands();
        }
    }

    protected void parseIf() {
        mustbe(TK.IF);
        parseGuardedCommands();
        mustbe(TK.FI);
    }

    protected void parseId() {
        mustbe(TK.ID);
    }

    protected void parseLe() {
        mustbe(TK.LE);
    }

    protected void parseLt() {
        mustbe(TK.LT);
    }

    protected void parseMinus() {
        mustbe(TK.MINUS);
    }

    protected void parseMultOp() {
        switch (tok.kind) {
            case TIMES:
                parseTimes();
                break;
            case DIVIDE:
                parseDivide();
                break;
            default:
                parseError("multop");
                break;
        }
    }

    protected void parseNe() {
        mustbe(TK.NE);
    }

    protected void parseNumber() {
        mustbe(TK.NUM);
    }

    protected void parseParenthesized() {
        mustbe(TK.LPAREN);
        parseExpression();
        mustbe(TK.RPAREN);
    }

    protected void parsePlus() {
        mustbe(TK.PLUS);
    }

    protected void parsePrint() {
        mustbe(TK.PRINT);
        parseExpression();
    }

    protected void parseRelOp() {
        switch (tok.kind) {
            case EQ:
                parseEq();
                break;
            case NE:
                parseNe();
                break;
            case LT:
                parseLt();
                break;
            case GT:
                parseGt();
                break;
            case LE:
                parseLe();
                break;
            case GE:
                parseGe();
                break;
            default:
                parseError("relop");
                break;
        }
    }

    protected void parseSimple() {
        parseTerm();
        while (isAddOp()) {
            parseAddOp();
            parseTerm();
        }
    }

    protected void parseSt() {
        mustbe(TK.ST);
    }

    protected void parseStatement() {
        if (isAssignment()) {
            parseAssignment();
        } else if (isPrint()) {
            parsePrint();
        } else if (isIf()) {
            parseIf();
        } else if (isDo()) {
            parseDo();
        } else if (isFa()) {
            parseFa();
        } else {
            parseError("statement");
        }
    }

    protected void parseStatementList() {
        while (isStatement()) {
            parseStatement();
        }
    }

    protected void parseTerm() {
        parseFactor();
        while (isMultOp()) {
            parseMultOp();
            parseFactor();
        }
    }

    protected void parseTimes() {
        mustbe(TK.TIMES);
    }

    protected boolean isAddOp() {
        return is(TK.PLUS) || is(TK.MINUS);
    }

    protected boolean isAssignment() {
        return is(TK.ID);
    }

    protected boolean isBox() {
        return is(TK.BOX);
    }

    protected boolean isDeclaration() {
        return is(TK.VAR);
    }

    protected boolean isDo() {
        return is(TK.DO);
    }

    protected boolean isElse() {
        return is(TK.ELSE);
    }

    protected boolean isFa() {
        return is(TK.FA);
    }

    protected boolean isId() {
        return is(TK.ID);
    }

    protected boolean isIf() {
        return is(TK.IF);
    }

    protected boolean isMultOp() {
        return is(TK.TIMES) || is(TK.DIVIDE);
    }

    protected boolean isNumber() {
        return is(TK.NUM);
    }

    protected boolean isParenthesized() {
        return is(TK.LPAREN);
    }

    protected boolean isPrint() {
        return is(TK.PRINT);
    }

    protected boolean isRelOp() {
        return is(TK.EQ) ||
               is(TK.NE) ||
               is(TK.LT) ||
               is(TK.GT) ||
               is(TK.LE) ||
               is(TK.GE);
    }

    protected boolean isSt() {
        return is(TK.ST);
    }

    protected boolean isStatement() {
        return isAssignment() || isPrint() || isIf() || isDo() || isFa();
    }

    // you'll need to add a bunch of methods here

    // is current token what we want?
    protected boolean is(TK tk) {
        return tk == tok.kind;
    }

    // ensure current token is tk and skip over it.
    protected void mustbe(TK tk) {
        if (!is(tk)) {
            System.err.println("mustbe: want " + tk + ", got " + tok);
            parseError("missing token (mustbe)");
        }
        scan();
    }

    protected void parseError(String msg) {
        System.err.println("can't parse: line " + tok.lineNumber + " " + msg);
        System.exit(1);
    }
}
