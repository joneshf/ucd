import java.util.ArrayDeque;
import java.util.ArrayList;

public class References extends Symbol {

    protected class Reference {

        private Token token;
        private int depth;
        private ArrayList<Integer> assigned = new ArrayList<Integer>();
        private ArrayList<Integer> used = new ArrayList<Integer>();

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
            return formatWith(assigned);
        }

        private String formatUsed() {
            return formatWith(used);
        }

        private String formatWith(ArrayList<Integer> al) {
            int consecutive = 0;
            StringBuilder str = new StringBuilder();
            for (int i = 0; i < al.size(); ++i) {
                if (i + 1 < al.size() && al.get(i+1) == al.get(i)) {
                    ++consecutive;
                } else if (consecutive > 0) {
                    str.append(al.get(i) + "(" + consecutive + ")");
                } else {
                    str.append(al.get(i));
                }
            }

            return str.toString();
        }

        public String toString() {
            return token.string +
                   "\n  declared on line " + token.lineNumber +
                   " at nesting depth " + this.depth +
                   "\n  assigned to on: " + formatAssigned() +
                   "\n  used on: " + formatUsed();
        }
    }

    protected ArrayDeque<Reference> refs;

    public References(Scan scanner) {
        super(scanner);
    }

    protected void parseDeclarations() {
        if (refs == null) {
            refs = new ArrayDeque<Reference>();
        }

        mustbe(TK.VAR);
        while (is(TK.ID)) {
            // We only need to check the first block for redeclarations.
            if (varInBlock(symbolTable.getFirst())) {
                // If it was declared, print an error.
                System.err.println(redeclared());
            } else {
                // New variable.
                symbolTable.getFirst().addFirst(tok);
            }
            refs.addFirst(new Reference(tok, symbolTable.size() - 1));
            scan();
        }
        mustbe(TK.RAV);
    }

    protected void parseProgram() {
        super.parseProgram();
        System.out.println(symbolTable);
        for (Reference ref : refs) {
            System.out.println(ref);
        }
    }
}
