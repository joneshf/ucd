import java.util.*;

public class References extends Symbol {

    protected class Reference {

        public Token token;
        public int depth;
        public ArrayList<Integer> assigned = new ArrayList<Integer>();
        public ArrayList<Integer> used = new ArrayList<Integer>();

        Reference (Token token, int depth) {
            this.token = token;
            this.depth = depth;
        }

        public void assignedOn(int line) {
            assigned.add(line);
        }

        public void usedOn(int line) {
            used.add(line);
        }

        private String formatAssigned() {
            if (assigned.size() == 0) {
                return "never assigned";
            } else {
                return "assigned to on:" + formatWith(assigned);
            }
        }

        private String formatUsed() {
            if (used.size() == 0) {
                return "never used";
            } else {
                return "used on:" + formatWith(used);
            }
        }

        private String formatWith(ArrayList<Integer> al) {
            int consecutive = 0;
            StringBuilder str = new StringBuilder();
            for (int i = 0; i < al.size(); ++i) {
                if (i + 1 < al.size() && al.get(i+1) == al.get(i)) {
                    ++consecutive;
                } else if (consecutive > 0) {
                    str.append(" " + al.get(i) + "(" + ++consecutive + ")");
                    consecutive = 0;
                } else {
                    str.append(" " + al.get(i));
                }
            }

            return str.toString();
        }

        public String toString() {
            return token.string +
                   "\n  declared on line " + token.lineNumber +
                   " at nesting depth " + this.depth +
                   "\n  " + formatAssigned() +
                   "\n  " + formatUsed();
        }
    }

    protected ArrayDeque<Reference> refs;

    public References(Scan scanner) {
        super(scanner);
    }

    protected void addAssign(Token t) {
        // Find the depth of the most recent var declaration.
        int depth = findDepth(t);
        // Find the reference at the correct depth.
        Reference ref = findReference(t, depth);
        // Assign it.
        ref.assignedOn(t.lineNumber);
    }

    protected void addUsed(Token t) {
        // Find the depth of the most recent var declaration.
        int depth = findDepth(t);
        // Find the reference at the correct depth.
        Reference ref = findReference(t, depth);
        // Used it.
        ref.usedOn(t.lineNumber);
    }

    protected int findDepth(Token t) {
        int depth = symbolTable.size() - 1;

        for (ArrayDeque<Token> block : symbolTable) {
            if (varInBlock(block, t)) {
                break;
            } else {
                --depth;
            }
        }

        return depth;
    }

    protected Reference findReference(Token t, int depth) {
        Reference r = new Reference(t, depth);

        for (Reference ref : refs) {
            if (ref.token.string.equals(t.string) && ref.depth <= depth) {
                r = ref;
                break;
            }
        }

        return r;
    }

    protected void parseAssignment() {
        Token t = new Token(tok.kind, tok.string, tok.lineNumber);
        mustbe(TK.ID);
        addAssign(t);
        mustbe(TK.ASSIGN);
        parseExpression();
    }

    protected void parseDeclarations() {
        if (refs == null) {
            refs = new ArrayDeque<Reference>();
        }

        mustbe(TK.VAR);
        while (is(TK.ID)) {
            // We only need to check the Last block for redeclarations.
            if (varInBlock(symbolTable.getFirst())) {
                // If it was declared, print an error.
                System.err.println(redeclared());
            } else {
                // New variable.
                symbolTable.getFirst().push(tok);
                refs.push(new Reference(tok, symbolTable.size() - 1));
            }
            scan();
        }
        mustbe(TK.RAV);
    }

    protected void parseFa() {
        mustbe(TK.FA);
        Token t = new Token(tok.kind, tok.string, tok.lineNumber);
        mustbe(TK.ID);
        addAssign(t);
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
            Token t = new Token(tok.kind, tok.string, tok.lineNumber);
            mustbe(TK.ID);
            addUsed(t);
        } else if (isNumber()) {
            parseNumber();
        } else {
            parseError("factor");
        }
    }

    protected void parseProgram() {
        super.parseProgram();
        if (tok.kind != TK.EOF) {
            parseError("junk after logical end of program");
            System.exit(1);
        } else if (refs != null) {
            while (!refs.isEmpty()) {
                Reference ref = refs.removeLast();
                System.err.println(ref);
            }
        }
    }
}
