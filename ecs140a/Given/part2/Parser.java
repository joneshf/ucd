public class Parser {
    public Parser(Scan scanner) {
        firstSet(scanner);
    }

    /**
     * Parses the first set of the grammar.
     * The only valid
     */
    public void firstSet(Scan scanner) {
        Token t = scanner.scan();
        switch (t.kind) {
            case VAR:
                System.out.println("Got a var");
                break;
            case PRINT:
                System.out.println("Got a print");
                parseExpression(scanner);
                break;
            case IF:
                System.out.println("Got a if");
                break;
            case DO:
                System.out.println("Got a do");
                break;
            case FA:
                System.out.println("Got a fa");
                break;
            case ID:
                System.out.println("Got a fa");
                break;
            default:
                System.out.println("Got something else");
                System.out.println(t);
        }
    }

    public void parseExpression(Scan scanner) {
        Token t = scanner.scan();

    }
}
