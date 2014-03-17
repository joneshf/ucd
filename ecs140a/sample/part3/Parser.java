/* *** This file is given as part of the programming assignment. *** */

import java.util.*;

public class Parser {


    // tok is global to all these parsing methods;
    // scan just calls the scanner's scan method and saves the result in tok.
    private Token tok; // the current token
    private int block_number; //This is the block number that we are currently in
    ArrayList <block_node> stack = new ArrayList <block_node>(); //Stack frame for symbol table


    private void scan() {
        tok = scanner.scan();
    }

    private Scan scanner;
    Parser(Scan scanner) {
        this.scanner = scanner;
        this.block_number = 0;

        block_node temp = new block_node(block_number);
        stack.add(temp);
        
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
            check_scope(tok.string);
            scan();
        }
        mustbe(TK.RAV);
    }
    private void statement_list(){
       while(is(TK.ID) || is(TK.PRINT) || is(TK.IF) || is(TK.DO) || is(TK.FA))          // what condition goes here
          statement();
    
    }
    private void statement(){
        if(is(TK.ID)){
          check_declared(tok.string);
          assignment();
        }
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
        check_declared(tok.string);
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
        else if(is(TK.ID)){
          check_declared(tok.string);
          mustbe(TK.ID);
        }
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
       push();
       block();
       pop();
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
       check_declared(tok.string);
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


    //***************************************
    //This is the code from part 3. 
    //***************************************

    //This checks to see if the variable being declared has been declared in the current scope or not.
    private void check_scope(String variable_name){
      
      for(int index = stack.get(block_number).variable_storage.size() - 1; 
          index > -1; index--){
      
        if(variable_name.equals(stack.get(block_number).variable_storage.get(index).variable_name)) {

          System.err.println("variable " + variable_name + " is redeclared on line " 
                             + tok.lineNumber);

          return;
        }//if the variable has already been declared

      }//for loop; searches through scope for variable.
      
    
      stack.get(block_number).variable_storage.add(new Variable_Node(variable_name, block_number));
      
    }//check_scope

    //This checks to see if the variable being used is in the anywhere in the program.
    private void check_declared(String variable_name){

      for(int block_index = block_number; block_index > -1; block_index--){

        for(int array_index = stack.get(block_index).variable_storage.size() - 1;
            array_index > -1; array_index--){

          if(variable_name.equals(stack.get(block_index).variable_storage.get(array_index).variable_name)){
            return;
          }//if there variable has been declared

        }//for loop; increments through the variable_storage
      }//for loop; increaments through the different blocks starting at most recent

      //Since there is no match, this means that the variable hasn't been declared.
      //Exit program.

      System.err.println("undeclared variable " + variable_name + " on line " + tok.lineNumber);
      System.exit(1);
    }//check_declared

    private void push(){
      
      block_number++;
      block_node temp = new block_node(block_number);
      stack.add(temp);
      
    }

    private void pop(){

      stack.remove(block_number);
      block_number--; 

    }

}// Parser Class











