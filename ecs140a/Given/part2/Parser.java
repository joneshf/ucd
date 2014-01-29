/* *** This file is given as part of the programming assignment. *** */

public class Parser {


    // tok is global to all these parsing methods;
    // scan just calls the scanner's scan method and saves the result in tok.
    private Token tok; // the current token
    private void scan() {
        tok = scanner.scan();
    }

    private Scan scanner;
    Parser(Scan scanner) {
        this.scanner = scanner;
        scan();
        parseProgram();
        if (tok.kind != TK.EOF)
            parseError("junk after logical end of program");
    }

    private void parseProgram() {
        parseBlock();
    }

    private void parseAddOp() {
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

    private void parseAssignment() {
        mustbe(TK.ID);
        mustbe(TK.ASSIGN);
        parseExpression();
    }

    private void parseBlock() {
        // Optional declarations.
        if (isDeclaration()) {
            parseDeclarations();
        }
        parseStatementList();
    }

    private void parseBox() {
        mustbe(TK.BOX);
    }

    private void parseCommands() {
        mustbe(TK.ARROW);
        parseBlock();
    }

    private void parseDeclarations() {
        mustbe(TK.VAR);
        while (is(TK.ID)) {
            scan();
        }
        mustbe(TK.RAV);
    }

    private void parseDivide() {
        mustbe(TK.DIVIDE);
    }

    private void parseDo() {
        mustbe(TK.DO);
        parseGuardedCommands();
        mustbe(TK.OD);
    }

    private void parseElse() {
        mustbe(TK.ELSE);
    }

    private void parseEq() {
        mustbe(TK.EQ);
    }

    private void parseExpression() {
        parseSimple();
        if (isRelOp()) {
            parseRelOp();
            parseSimple();
        }
    }

    private void parseFa() {
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

    private void parseFactor() {
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

    private void parseGe() {
        mustbe(TK.GE);
    }

    private void parseGt() {
        mustbe(TK.GT);
    }

    private void parseGuardedCommand() {
        parseExpression();
        parseCommands();
    }

    private void parseGuardedCommands() {
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

    private void parseIf() {
        mustbe(TK.IF);
        parseGuardedCommands();
        mustbe(TK.FI);
    }

    private void parseId() {
        mustbe(TK.ID);
    }

    private void parseLe() {
        mustbe(TK.LE);
    }

    private void parseLt() {
        mustbe(TK.LT);
    }

    private void parseMinus() {
        mustbe(TK.MINUS);
    }

    private void parseMultOp() {
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

    private void parseNe() {
        mustbe(TK.NE);
    }

    private void parseNumber() {
        mustbe(TK.NUM);
    }

    private void parseParenthesized() {
        mustbe(TK.LPAREN);
        parseExpression();
        mustbe(TK.RPAREN);
    }

    private void parsePlus() {
        mustbe(TK.PLUS);
    }

    private void parsePrint() {
        mustbe(TK.PRINT);
        parseExpression();
    }

    private void parseRelOp() {
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

    private void parseSimple() {
        parseTerm();
        while (isAddOp()) {
            parseAddOp();
            parseTerm();
        }
    }

    private void parseSt() {
        mustbe(TK.ST);
    }

    private void parseStatement() {
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

    private void parseStatementList() {
        while (isStatement()) {
            parseStatement();
        }
    }

    private void parseTerm() {
        parseFactor();
        while (isMultOp()) {
            parseMultOp();
            parseFactor();
        }
    }

    private void parseTimes() {
        mustbe(TK.TIMES);
    }

    private boolean isAddOp() {
        return is(TK.PLUS) || is(TK.MINUS);
    }

    private boolean isAssignment() {
        return is(TK.ID);
    }

    private boolean isBox() {
        return is(TK.BOX);
    }

    private boolean isDeclaration() {
        return is(TK.VAR);
    }

    private boolean isDo() {
        return is(TK.DO);
    }

    private boolean isElse() {
        return is(TK.ELSE);
    }

    private boolean isFa() {
        return is(TK.FA);
    }

    private boolean isId() {
        return is(TK.ID);
    }

    private boolean isIf() {
        return is(TK.IF);
    }

    private boolean isMultOp() {
        return is(TK.TIMES) || is(TK.DIVIDE);
    }

    private boolean isNumber() {
        return is(TK.NUM);
    }

    private boolean isParenthesized() {
        return is(TK.LPAREN);
    }

    private boolean isPrint() {
        return is(TK.PRINT);
    }

    private boolean isRelOp() {
        return is(TK.EQ) ||
               is(TK.NE) ||
               is(TK.LT) ||
               is(TK.GT) ||
               is(TK.LE) ||
               is(TK.GE);
    }

    private boolean isSt() {
        return is(TK.ST);
    }

    private boolean isStatement() {
        return isAssignment() || isPrint() || isIf() || isDo() || isFa();
    }

    // you'll need to add a bunch of methods here

    // is current token what we want?
    private boolean is(TK tk) {
        return tk == tok.kind;
    }

    // ensure current token is tk and skip over it.
    private void mustbe(TK tk) {
        if (!is(tk)) {
            System.err.println("mustbe: want " + tk + ", got " + tok);
            parseError("missing token (mustbe)");
        }
        scan();
    }

    private void parseError(String msg) {
        System.err.println("can't parse: line " + tok.lineNumber + " " + msg);
        System.exit(1);
    }
}
