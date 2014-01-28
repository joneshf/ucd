public class Parser {
    public Parser(Scan scanner) {
        Validation<String, Token> parsed = parse(scanner);
        if (parsed.isValid()) {
            // System.out.println("It's valid!");
            // System.out.println(parsed.value());
        } else {
            System.err.println(parsed.value());
            System.exit(1);
        }
    }

    /**
     * Parses the first set of the grammar.
     * The only valid choices are `var`, `print`, `if`, `do`, `fa`, and `id`
     *
     * Overloaded to account for tokens already being read.
     *
     * @param scanner The scanner we're reading tokens from.
     * @param t       The token we already tried to read.
     */
    public Validation<String, Token> parse(Scan scanner, Token t) {
        do {
            switch (t.kind) {
                case PRINT:
                    return parseExpression(scanner);
                case VAR:
                    return parseDeclaration(scanner);
                case EOF:
                    return Validation.valid(t);
                default:
                    return Validation.invalid("can't parse: line "+t.lineNumber+
                                              " junk after logical end of"+
                                              " program");
            }
        } while (true);
    }

    /**
     * Parses the first set of the grammar.
     * The only valid choices are `var`, `print`, `if`, `do`, `fa`, and `id`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    public Validation<String, Token> parse(Scan scanner) {
        Token t = scanner.scan();
        return parse(scanner, t);
    }

    /**
     * Parses declarations.
     * Declarations are `var {id} rav`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseDeclaration(Scan scanner) {
        // Snag the next token.
        Token next = scanner.scan();
        // We can have zero or more `id`'s
        while (next.kind == TK.ID) {
            next = scanner.scan();
        }

        // The token wasn't an id, it better be a `rav`.
        if (next.kind == TK.RAV) {
            return Validation.valid(next);
        } else {
            return Validation.invalid("Expected rav");
        }
    }

    /**
     * Parses expressions.
     * Expressions are `simple` followed by zero or more `relop simple`.
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseExpression(Scan scanner) {
        // We need the first thing to be a simple.
        Validation<String, Token> parsed = parseSimple(scanner);
        if (parsed.isInvalid()) {
            return parsed;
        } else if (((Token) parsed.value()).kind == TK.EOF) {
            return parsed;
        } else {
            return manyExpression(scanner);
        }
    }

    /**
     * Parses factors.
     * Factors are parenthesized expressions, id's or numbers.
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseFactor(Scan scanner) {
        // Find out what the first token is.
        Token t = scanner.scan();
        switch (t.kind) {
            case LPAREN:
                // Grab the expression.
                Validation<String, Token> parsed = parseExpression(scanner);
                if (parsed.isInvalid()) return parsed;
                // And the right paren.
                if (scanner.scan().kind == TK.LPAREN) return parsed;
                else return Validation.invalid("Missing right paren");
            case ID:
            case NUM:
                return Validation.valid(t);
            default:
                return Validation.invalid(t.toString());
        }
    }

    /**
     * Parses simples.
     * Simples are `term`'s followed by zero or more `addop term`'s
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseSimple(Scan scanner) {
        // We need the first thing to be a term.
        Validation<String, Token> parsed = parseTerm(scanner);
        if (parsed.isInvalid()) {
            return parsed;
        } else if (((Token) parsed.value()).kind == TK.EOF) {
            return parsed;
        } else {
            return manySimple(scanner);
        }
    }

    /**
     * Parses terms.
     * Terms are `factor`'s followed by zero or more `multop factor`'s
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseTerm(Scan scanner) {
        // // We need the first thing to be a factor.
        Validation<String, Token> parsed = parseFactor(scanner);
        if (parsed.isInvalid()) {
            return parsed;
        } else if (((Token) parsed.value()).kind == TK.EOF) {
            return parsed;
        } else {
            return manyTerm(scanner);
        }
    }

    /**
     * Parses zero or more `expression`'s
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> manyExpression(Scan scanner) {
        // We can have zero or more `relop simple`'s
        Token next = scanner.scan();
        System.out.println("Parsing manyExpression.");
        System.out.println(next.toString());
        switch (next.kind) {
            case PLUS:
            case MINUS:
                return parseExpression(scanner);
            default:
                return parse(scanner, next);
        }
    }

    /**
     * Parses zero or more `simple`'s
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> manySimple(Scan scanner) {
        // We can have zero or more `addop term`'s
        Token next = scanner.scan();
        switch (next.kind) {
            case EQ:
            case NE:
            case LT:
            case GT:
            case LE:
            case GE:
                return parseSimple(scanner);
            default:
                return parse(scanner, next);
        }
    }

    /**
     * Parses zero or more `term`'s
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> manyTerm(Scan scanner) {
        // We can have zero or more `multop factor`'s
        Token next = scanner.scan();
        switch (next.kind) {
            case TIMES:
            case DIVIDE:
                return parseTerm(scanner);
            default:
                return parse(scanner, next);
        }
    }
}
