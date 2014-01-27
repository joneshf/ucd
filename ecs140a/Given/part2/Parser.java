public class Parser {
    public Parser(Scan scanner) {
        Validation<String, String> parsed = parse(scanner);
        if (parsed.isValid()) {
            System.out.println("It's valid!");
            System.out.println(parsed.valid);
        } else {
            System.out.println("No bueno");
            System.out.println(parsed.value());
        }
    }

    /**
     * Parses the first set of the grammar.
     * The only valid choices are `var`, `print`, `if`, `do`, `fa`, and `id`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    public Validation<String, String> parse(Scan scanner) {
        Token t = scanner.scan();
        switch (t.kind) {
            // case VAR:
            //     System.out.println("Got a var");
            //     break;
            case PRINT:
                System.out.println("Got a print");
                return parseExpression(scanner);
            // case IF:
            //     System.out.println("Got a if");
            //     break;
            // case DO:
            //     System.out.println("Got a do");
            //     break;
            // case FA:
            //     System.out.println("Got a fa");
            //     break;
            // case ID:
            //     System.out.println("Got a fa");
            //     break;
            default:
                System.out.println("Got something else");
                return Validation.invalid(t.toString());
        }
    }

    /**
     * Parses expressions.
     * Expressions are `simple` followed by zero or more `relop simple`.
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, String> parseExpression(Scan scanner) {
        // // We need the first thing to be a simple.
        // Validation<String, String> parsed = parseSimple(scanner);
        // if (parsed.isValid()) {
        //     return parsed;
        // }
        return parseSimple(scanner);
        // We can have zero or more `relop simple`'s
    }

    /**
     * Parses simples.
     * Simples are `term`'s followed by zero or more `addop term`'s
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, String> parseSimple(Scan scanner) {
        // // We need the first thing to be a term.
        // Validation<String, String> parsed = parseTerm(scanner);
        // if (parsed.isValid()) {
        //     return parsed;
        // }
        return parseTerm(scanner);
    }

    /**
     * Parses terms.
     * Terms are `factor`'s followed by zero or more `multop factor`'s
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, String> parseTerm(Scan scanner) {
        // // We need the first thing to be a factor.
        // Validation<String, String> parsed = parseFactor(scanner);
        // if (parsed.isValid()) {
        //     return parsed;
        // }
        return parseFactor(scanner);
    }

    /**
     * Parses factors.
     * Factors are parenthesized expressions, id's or numbers.
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, String> parseFactor(Scan scanner) {
        // Find out what the first token is.
        Token t = scanner.scan();
        switch (t.kind) {
            case LPAREN:
            case ID:
            case NUM:
                return Validation.valid(t.toString());
            default:
                return Validation.invalid(t.toString());
        }
    }
}
