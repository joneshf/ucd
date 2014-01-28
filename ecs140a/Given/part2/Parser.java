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
        Validation<String, Token> parsed;
        do {
            switch (t.kind) {
                case ASSIGN:
                    parsed = parseExpression(scanner);
                    break;
                case DO:
                    parsed = parseDo(scanner);
                    break;
                case FA:
                    parsed = parseFa(scanner);
                    break;
                case ID:
                    parsed = Validation.valid(t);
                    break;
                case IF:
                    parsed = parseIf(scanner);
                    break;
                case PRINT:
                    parsed = parseExpression(scanner);
                    break;
                case VAR:
                    parsed = parseDeclaration(scanner);
                    break;
                case EOF:
                    return Validation.valid(t);
                default:
                    System.out.println("WTF " + t);
                    System.out.println("WTF " + scanner.scan());
                    return Validation.invalid("can't parse: line "+t.lineNumber+
                                              " junk after logical end of"+
                                              " program");
            }
            if (parsed.isInvalid()) {
                return parsed;
            } else if (((Token) parsed.value()).kind == TK.EOF) {
                return parsed;
            }
            t = scanner.scan();
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
     * Parses assignment.
     * Assignment is `id ':=' expression`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseAssignment(Scan scanner) {
        // Snag the next token.
        Token next = scanner.scan();
        // It's gotta be an id.
        if (next.kind == TK.ID) {
            next = scanner.scan();
            if (next.kind == TK.ASSIGN) {
                return parseExpression(scanner);
            } else {
                return Validation.invalid("Expected assign.");
            }
        } else {
            return Validation.invalid("Expected id.");
        }
    }

    /**
     * Parses boxed guarded commands.
     * boxedGuards are zero or more `'[]' guardedCommand`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseBoxedGuards(Scan scanner) {
        // We basically want many of these things.
        return manyBoxedGuards(scanner);
    }

    /**
     * Parses commands.
     * Commands are `'->' block`
     * A block is the start of parsing.
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseCommands(Scan scanner) {
        // Snag the next token.
        Token next = scanner.scan();
        // It's gotta be an arrow.
        if (next.kind == TK.ARROW) {
            return parse(scanner);
        } else {
            return Validation.invalid("Expected arrow.");
        }
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
     * Parses do
     * Do is `do guardedCommands od`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseDo(Scan scanner) {
        // We need the guardedCommands to be valid.
        Validation<String, Token> parsed = parseGuardedCommands(scanner);
        if (parsed.isInvalid()) {
            return parsed;
        } else if (((Token) parsed.value()).kind == TK.EOF) {
            return parsed;
        } else if (scanner.scan().kind == TK.OD) {
            // We need to have `od`.
            return parse(scanner);
        } else {
            // We didn't have `od`.
            return Validation.invalid("Expected od");
        }
    }

    /**
     * Parses else.
     * Elses are optional `else commands`.
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseElse(Scan scanner, Validation<String, Token> token) {
        Token next = (Token) token.value();
        // We either need to have an else followed by commands,
        if (next.kind == TK.ELSE) {
            return parseCommands(scanner);
        } else {
            // or nothing matching.
            return parse(scanner, next);
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
     * Parses fa
     * Fa is `fa assignment to expression [st expression] commands af`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseFa(Scan scanner) {
        // We need the assignment to be valid.
        Validation<String, Token> parsed = parseAssignment(scanner);
        if (parsed.isInvalid()) {
            return parsed;
        } else if (((Token) parsed.value()).kind == TK.EOF) {
            return parsed;
        } else if (scanner.scan().kind == TK.TO) {
            // We need to have `to`.
            // We need an expression.
            parsed = parseExpression(scanner);
            if (parsed.isInvalid()) {
                return parsed;
            } else if (((Token) parsed.value()).kind == TK.EOF) {
                return parsed;
            } else {
                Token next = scanner.scan();
                // We might have `st` followed by another expression.
                if (next.kind == TK.ST) {
                    parsed = parseExpression(scanner);
                    if (parsed.isInvalid()) {
                        return parsed;
                    } else if (((Token) parsed.value()).kind == TK.EOF) {
                        return parsed;
                    }
                }

                // We need commands followed by `fa`.
                parsed = parseCommands(scanner);
                if (parsed.isInvalid()) {
                    return parsed;
                } else if (((Token) parsed.value()).kind == TK.EOF) {
                    return parsed;
                } else if (scanner.scan().kind == TK.AF) {
                    // We made it!
                    return parse(scanner);
                } else {
                    // We didn't have `af`.
                    return Validation.invalid("Expected af");
                }
            }
        } else {
            // We didn't have `to`.
            return Validation.invalid("Expected to");
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
     * Parses guardedCommand
     * GuardedCommand is: `expression commands`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseGuardedCommand(Scan scanner) {
        // The first thing needs to be an expression.
        Validation<String, Token> parsed = parseExpression(scanner);
        if (parsed.isInvalid()) {
            return parsed;
        } else if (((Token) parsed.value()).kind == TK.EOF) {
            return parsed;
        } else {
            // And we need commands.
            parsed = parseCommands(scanner);
            if (parsed.isInvalid()) {
                return parsed;
            } else if (((Token) parsed.value()).kind == TK.EOF) {
                return parsed;
            } else {
                return parse(scanner);
            }
        }
    }
    /**
     * Parses guardedCommands
     * GuardedCommands are:
     *      `guardedCommand {'[]' guardedCommand} [else commands]`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseGuardedCommands(Scan scanner) {
        // The first thing needs to be a guardedCommand.
        Validation<String, Token> parsed = parseGuardedCommand(scanner);
        if (parsed.isInvalid()) {
            return parsed;
        } else if (((Token) parsed.value()).kind == TK.EOF) {
            return parsed;
        } else {
            // We can have many boxedGuardedCommands
            parsed = parseBoxedGuards(scanner);
            if (parsed.isInvalid()) {
                return parsed;
            } else if (((Token) parsed.value()).kind == TK.EOF) {
                return parsed;
            } else {
                // Followed by an optional else.
                return parseElse(scanner, parsed);
            }
        }
    }

    /**
     * Parses if
     * If is `if guardedCommands fi`
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> parseIf(Scan scanner) {
        // We need the guardedCommands to be valid.
        Validation<String, Token> parsed = parseGuardedCommands(scanner);
        if (parsed.isInvalid()) {
            return parsed;
        } else if (((Token) parsed.value()).kind == TK.EOF) {
            return parsed;
        } else if (scanner.scan().kind == TK.FI) {
            // We need to have `fi`.
            return parse(scanner);
        } else {
            // We didn't have `fi`.
            return Validation.invalid("Expected fi");
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
     * Parses zero or more `boxedGuards`'s
     *
     * @param scanner The scanner we're reading tokens from.
     */
    private Validation<String, Token> manyBoxedGuards(Scan scanner) {
        // We can have zero or more `'[]' guardedCommand`'s
        Token next = scanner.scan();
        if (next.kind == TK.BOX) {
            Validation<String, Token> parsed = parseGuardedCommand(scanner);
            if (parsed.isInvalid()) {
                return parsed;
            } else if (((Token) parsed.value()).kind == TK.EOF) {
                return parsed;
            } else {
                return manyBoxedGuards(scanner);
            }
        } else {
            // We didn't have a box,
            // let's just throw back the token to use for more parsing.
            return Validation.valid(next);
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
        switch (next.kind) {
            case EQ:
            case NE:
            case LT:
            case GT:
            case LE:
            case GE:
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
            case PLUS:
            case MINUS:
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
