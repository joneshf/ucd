import java.util.ArrayDeque;

public class Symbol extends Parser {
    protected ArrayDeque<ArrayDeque<Token>> symbolTable;

    public Symbol(Scan scanner) {
        super(scanner);
    }

    protected void parseBlock() {
        if (symbolTable == null) {
            symbolTable = new ArrayDeque<ArrayDeque<Token>>();
        }
        // We just need to add a block when we start.
        symbolTable.push(new ArrayDeque<Token>());
        super.parseBlock();
        // We  need to remove a block when we end.
        symbolTable.pop();
    }

    protected void parseDeclarations() {
        mustbe(TK.VAR);
        while (is(TK.ID)) {
            // We only need to check the first block for redeclarations.
            if (varInBlock(symbolTable.getFirst())) {
                // If it was declared, print an error.
                System.err.println(redeclared());
            } else {
                // New variable.
                symbolTable.getFirst().push(tok);
            }
            scan();
        }
        mustbe(TK.RAV);
    }

    protected boolean isUndeclared() {
        for (ArrayDeque<Token> block : symbolTable) {
            if (varInBlock(block)) {
                return false;
            }
        }

        return true;
    }

    protected boolean varInBlock(ArrayDeque<Token> block) {
        return varInBlock(block, tok);
    }

    protected boolean varInBlock(ArrayDeque<Token> block, Token token) {
        for (Token t : block) {
            if (t.string.equals(token.string)) {
                return true;
            }
        }

        return false;
    }

    protected String redeclared() {
        return "variable " + tok.string +
               " is redeclared on line " + tok.lineNumber;
    }

    protected String undeclared() {
        return "undeclared variable " + tok.string +
               " on line " + tok.lineNumber;
    }

    // Let's override mustbe to validate id's.
    protected void mustbe(TK tk) {
        if (!is(tk)) {
            System.err.println("mustbe: want " + tk + ", got " + tok);
            parseError("missing token (mustbe)");
        }
        if (tk == TK.ID && isUndeclared()) {
            System.err.println(undeclared());
            System.exit(1);
        }
        scan();
    }
}
