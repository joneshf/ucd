import java.io.*;
import java.util.*;

public class CG extends References {

    protected int doLabelCounter;
    protected int indent;
    protected String indentation;

    public CG(Scan scanner) {
        super(scanner);
    }

    protected String getIndent() {
        StringBuilder str = new StringBuilder();
        for (int i = 0; i < indent; ++i) {
            str.append(indentation);
        }

        return str.toString();
    }

    protected void cLine() {
        cLine("");
    }

    protected void cLine(String cString) {
        cStrIndent(cString+"\n");
    }

    protected void cStrIndent(String cString) {
        cStr(getIndent()+cString);
    }

    protected void cStr(String cString) {
        System.out.print(cString);
    }

    protected void parseProgram() {
        doLabelCounter = 0;
        indent = 0;
        indentation = "    ";
        cLine("#include <stdio.h>");
        cLine();
        cLine("int main (void) {");
        ++indent;
        super.parseProgram();
        cLine("return 0;");
        indent = 0;
        cLine("}");
    }

    protected void parseAssignment() {
        cStrIndent("");
        super.parseAssignment();
        cStr(";\n");
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
                scan();
            } else {
                // New variable.
                symbolTable.getFirst().push(tok);
                refs.push(new Reference(tok, symbolTable.size() - 1));
                mustbe(TK.ID);
                cStr(" = -12345");
                if (is(TK.ID)) {
                    cStr(", ");
                }
            }
        }
        mustbe(TK.RAV);
    }

    protected void parseDo() {
        cLine("doLabel"+doLabelCounter+":");
        int oldCounter = doLabelCounter++;
        mustbe(TK.DO);
        parseGuardedDoCommands(oldCounter);
        mustbe(TK.OD);
    }

    protected void parseFa() {
        mustbe(TK.FA);
        Token t = new Token(tok.kind, tok.string, tok.lineNumber);
        mustbe(TK.ID);
        addAssign(t);
        mustbe(TK.ASSIGN);
        parseExpression();
        mustbe(TK.TO);
        cStr("x_"+t.string+" <= ");
        parseExpression();
        cStr("; ++x_"+t.string+") {\n");
        ++indent;
        if (isSt()) {
            parseSt();
            parseExpression();
            cStr(") {\n");
            ++indent;
            parseCommands();
            --indent;
            cLine("}");
        } else {
            parseCommands();
        }
        --indent;
        mustbe(TK.AF);
    }

    protected void parseGuardedCommand() {
        parseExpression();
        cStr(") {\n");
        ++indent;
        parseCommands();
        --indent;
    }

    protected void parseGuardedCommands() {
        parseGuardedCommand();
        while (isBox()) {
            parseBox();
            parseGuardedCommand();
        }
        if (isElse()) {
            parseElse();
            ++indent;
            parseCommands();
            --indent;
        }
    }

    protected void parseGuardedDoCommand(int labelCounter) {
        // We need the contents of the expression.
        // So redirect stdout to a stream to capture it, then use it later.
        // Save the old stdout first.
        PrintStream oldOut = System.out;
        // Create a byte array to hold our bytes.
        ByteArrayOutputStream b = new ByteArrayOutputStream();
        // Make a stream to grab the system out.
        PrintStream newOut = new PrintStream(b);
        // Set the stream up.
        System.setOut(newOut);
        parseExpression();
        // Get the actual string.
        System.out.flush();
        String condition = b.toString();
        // Go back to the old out.
        System.setOut(oldOut);
        // Write the condition to stdout like nothing happened.
        cStr(condition);
        cStr(") {\n");
        ++indent;
        cLine("do {");
        ++indent;
        parseCommands();
        --indent;
        cLine("} while ("+condition+");");
        cLine("goto doLabel"+labelCounter+";");
        --indent;
    }

    protected void parseGuardedDoCommands(int labelCounter) {
        parseGuardedDoCommand(labelCounter);
        while (isBox()) {
            parseBox();
            parseGuardedDoCommand(labelCounter);
        }
        if (isElse()) {
            parseElse();
            ++indent;
            parseCommands();
            --indent;
        }
    }

    protected void parsePrint() {
        super.parsePrint();
        cStr(");\n");
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
        // We can do everything from here.
        switch (tk) {
            case VAR:
                cStrIndent("int ");
                break;
            case RAV:
                cStr(";\n");
                break;
            case IF:
            case DO:
                cStrIndent("if (");
                break;
                // cStrIndent("while (");
                // break;
            case ELSE:
                cLine("} else {");
                break;
            case FA:
                cStrIndent("for (");
                break;
            case TO:
                cStr("; ");
                break;
            case ST:
                cStrIndent("if (");
                break;
            case PRINT:
                cStrIndent("printf(\"%d\\n\", ");
                break;
            case FI:
            case OD:
            case AF:
                cLine("}");
                break;
            case ARROW:
                cStr("");
                break;
            case BOX:
                cStrIndent("} else if (");
                break;
            case ASSIGN:
                cStr(" = ");
                break;
            case NE:
                cStr(" != ");
                break;
            case EQ:
                cStr(" == ");
                break;
            case PLUS:
            case MINUS:
            case TIMES:
            case DIVIDE:
            case LT:
            case GT:
            case LE:
            case GE:
                cStr(" "+tok.string+" ");
                break;
            case ID:
                cStr("x_"+tok.string);
                break;
            default:
                cStr(tok.string);
                break;
        }
        scan();
    }
}
