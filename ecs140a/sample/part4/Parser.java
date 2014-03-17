/* *** This file is given as part of the programming assignment. *** */

import java.util.*;

public class Parser {


    // tok is global to all these parsing methods;
    // scan just calls the scanner's scan method and saves the result in tok.
    private int previous; //used for part 4.
    private Token tok; // the current token
    private int block_number; //This is the block number that we are currently in
    ArrayList <block_node> stack = new ArrayList <block_node>(); //Stack frame for symbol table
    ArrayList <Variable_Reference> Variable_Tally = new ArrayList <Variable_Reference>();//part4

    private void scan() {
        tok = scanner.scan();
    }

    private Scan scanner;
    Parser(Scan scanner) {

        this.scanner = scanner;
        this.block_number = 0;
        this.previous = -1;

        block_node temp = new block_node(block_number);
        stack.add(temp);
        
        scan();
        program();
        if( tok.kind != TK.EOF )
            parse_error("junk after logical end of program");
        print_variables();
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
            if(check_scope(tok.string))
              add_variable();
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
        add_assigned_on();
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
          add_used_on();
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
      pop_Variable_Tally();
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
       add_assigned_on();
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
    private boolean check_scope(String variable_name){
      
      for(int index = stack.get(block_number).variable_storage.size() - 1; 
          index > -1; index--){
      
        if(variable_name.equals(stack.get(block_number).variable_storage.get(index).variable_name)) {

          System.err.println("variable " + variable_name + " is redeclared on line " 
                             + tok.lineNumber);

          return false;
        }//if the variable has already been declared

      }//for loop; searches through scope for variable.
      
    
      stack.get(block_number).variable_storage.add(new Variable_Node(variable_name, block_number));
      return true;
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

    //These functions are for part 4. Complicated.

    //This function adds to Variable_Tally the name, depth, and line number when a
    //variable is assigned.

    private void add_variable(){
      Variable_Tally.add(new Variable_Reference(tok.string, block_number,
                         tok.lineNumber, previous));
      previous = Variable_Tally.size() - 1;

    }

    private void add_used_on(){

      int index = search();
      Variable_Tally.get(index).used_on.add(tok.lineNumber);

    }

    private void add_assigned_on(){

      int index = search();
      Variable_Tally.get(index).assigned_on.add(tok.lineNumber);
    }

    //The search function searches for the correct slot to place the variable.
    private int search(){

      int index = previous;
      while(true){
        if(tok.string.equals(Variable_Tally.get(index).variable_name)){
          return index;
        }

        else
          index = Variable_Tally.get(index).previous;
      }

    }

    //When I pop, I change Variable Tally as Well
    //I keep going down the block until I reach a scope one less.
    private void pop_Variable_Tally(){

      int current_depth = block_number;
      int index = previous;

      while(true){
        int searching_depth = Variable_Tally.get(index).nesting_depth;
        
        if(searching_depth == -1){

          previous = 0;
          break;
        }  
        else if(searching_depth < current_depth){
          previous = index;
          break;
        } 
        else
          index = Variable_Tally.get(index).previous;
      }
    }

    private void print_variables(){

      //looping through every integer
      for(int index = 0; index < Variable_Tally.size(); index++){
        int duplicates = 0;
        System.out.println(Variable_Tally.get(index).variable_name);
        System.out.println("  declared on line " +
              Variable_Tally.get(index).line_declaration + " at nesting depth " + 
              Variable_Tally.get(index).nesting_depth);

              //printing out assigned to
              if(Variable_Tally.get(index).assigned_on.size() == 0){
                System.out.println( "  never assigned");
              }
              else{
                System.out.print("  assigned to on:");
                for(int aindex = 0; aindex < Variable_Tally.get(index).assigned_on.size(); aindex++){
                  
                  if((aindex < Variable_Tally.get(index).assigned_on.size() - 1) &&
                      (Variable_Tally.get(index).assigned_on.get(aindex) == 
                      Variable_Tally.get(index).assigned_on.get(aindex + 1))){

                      duplicates++;
                  }
                  else if(duplicates > 0){
                    System.out.print(" " + Variable_Tally.get(index).assigned_on.get(aindex) + 
                      "(" + (duplicates + 1) + ")");
                    duplicates = 0;
                  }
                  else
                    System.out.print(" " + Variable_Tally.get(index).assigned_on.get(aindex));

                }
                System.out.println();
              }
              //printing out used on
              if(Variable_Tally.get(index).used_on.size() == 0){
                System.out.println( "  never used");
              }
              else{
              System.out.print("  used on:");
                for(int uindex = 0; uindex < Variable_Tally.get(index).used_on.size(); uindex++){
                  
                  if((uindex < Variable_Tally.get(index).used_on.size() - 1) &&
                      (Variable_Tally.get(index).used_on.get(uindex) == 
                      Variable_Tally.get(index).used_on.get(uindex + 1))){

                      duplicates++;
                  }
                  else if(duplicates > 0){
                    System.out.print(" " + Variable_Tally.get(index).used_on.get(uindex) + 
                      "(" + (duplicates + 1) + ")");
                    duplicates = 0;
                  }
                  else
                    System.out.print(" " + Variable_Tally.get(index).used_on.get(uindex));

                }
                System.out.println();
            }
             //printing out used on

      }
    }
}// Parser Class











