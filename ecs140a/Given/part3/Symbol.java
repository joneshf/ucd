import java.util.ArrayDeque;
import java.util.Deque;

public class Symbol extends Parser {
    private ArrayDeque<ArrayDeque<Token>> symbolTable;

    public Symbol(Scan scanner) {
        super(scanner);
    }

    // We just need to add a block when we start.
    protected void parseBlock() {
        if (symbolTable == null) {
            symbolTable = new ArrayDeque<ArrayDeque<Token>>();
        }
        symbolTable.addFirst(new ArrayDeque<Token>());
        super.parseBlock();
        symbolTable.removeFirst();
    }

    protected void parseDeclarations() {
        mustbe(TK.VAR);
        while (is(TK.ID)) {
            boolean declared = false;
            for (ArrayDeque<Token> block : symbolTable) {
                if (varInBlock(block)) {
                    // Already in here.
                    declared = true;
                    break;
                }
            }
            if (declared) {
                // If it was declared, print an error.
                System.err.println(redeclared());
            } else {
                // New variable.
                symbolTable.getFirst().addFirst(tok);
            }
            scan();
        }
        mustbe(TK.RAV);
    }

    private boolean varInBlock(ArrayDeque<Token> block) {
        for (Token t : block) {
            if (t.string.equals(tok.string)) {
                return true;
            }
        }

        return false;
    }

    private String redeclared() {
        return "variable " + tok.string +
               " is redeclared on line " + tok.lineNumber;
    }
}
