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
        program();
        if( tok.kind != TK.EOF )
            parse_error("junk after logical end of program");
    }

    private void program() {
        block();
    }

    private void block() {
        // you'll need to add some code here
        if(is(TK.VAR))
           declarations();
        statement_list();
    }

    private void declarations() {
        mustbe(TK.VAR);
        while( is(TK.ID) ) {
            scan();
        }
        mustbe(TK.RAV);
    }
    private void statement_list(){
       while(is(TK.ID) || is(TK.PRINT) || is(TK.IF) || is(TK.DO) || is(TK.FA))          // what condition goes here
          statement();
    
    }
    private void statement(){
        if(is(TK.ID))
          assignment();
        else if(is(TK.PRINT))
          print();
        else if(is(TK.IF))
          if_statement();
        else if(is(TK.DO))
          do_statement();
        else if(is(TK.FA))
          fa();
        else
          parse_error("statement");
          
    }
    private void assignment(){
        mustbe(TK.ID);
        mustbe(TK.ASSIGN);
        expression();

    }
    private void expression(){
        simple();
        if( is(TK.EQ) || is(TK.NE) || is(TK.LT) || is(TK.GT) || is(TK.LE) || is(TK.GE)){
           scan();
           simple();
        }
    }
    private void simple(){
        term();
        while(is(TK.PLUS) || is(TK.MINUS)){
           scan();
           term();
        }
    }
    private void term(){
        factor();
        while(is(TK.TIMES) || is(TK.DIVIDE) ){
           scan();
           factor();
        }

    }
    private void factor(){
        if(is(TK.LPAREN)) {
          mustbe(TK.LPAREN);
          expression();
          mustbe(TK.RPAREN);

        }
        else if(is(TK.ID))
          mustbe(TK.ID);
        else if(is(TK.NUM))
          mustbe(TK.NUM);
        else
          parse_error("factor");
    }
    private void print(){
        mustbe(TK.PRINT);
        expression();
    }
    private void if_statement(){
        mustbe(TK.IF);
        guarded_commands();
        mustbe(TK.FI);
    }
    private void guarded_commands(){
       guarded_command();
       while(is(TK.BOX)){
          mustbe(TK.BOX);
          guarded_command();
       
       }
       if(is(TK.ELSE)){
          mustbe(TK.ELSE);
          commands();
       }
    }
    private void commands(){
       mustbe(TK.ARROW);
       block();
    }
    private void guarded_command(){
       expression();
       commands();
    }
    private void do_statement(){
       mustbe(TK.DO);
       guarded_commands();
       mustbe(TK.OD);
    }
    private void fa(){
       mustbe(TK.FA);
       mustbe(TK.ID);
       mustbe(TK.ASSIGN);
       expression();
       mustbe(TK.TO);
       expression();
       if(is(TK.ST)){
          mustbe(TK.ST);
          expression();
       }
       commands();
       mustbe(TK.AF);

    }
     //
     //
    // you'll need to add a bunch of methods here

    // is current token what we want?
    private boolean is(TK tk) {
        return tk == tok.kind;
    }

    // ensure current token is tk and skip over it.
    private void mustbe(TK tk) {
        if( ! is(tk) ) {
            System.err.println( "mustbe: want " + tk + ", got " +
                                    tok);
            parse_error( "missing token (mustbe)" );
        }
        scan();
    }

    private void parse_error(String msg) {
        System.err.println( "can't parse: line "
                            + tok.lineNumber + " " + msg );
        System.exit(1);
    }
}
