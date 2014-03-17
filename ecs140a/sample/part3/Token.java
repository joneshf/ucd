
import java.util.*;

public class Token {
    public TK kind;
    public String string;
    public int lineNumber;
    public Token(TK kind, String string, int lineNumber) {
        this.kind = kind;
        this.string = string;
        this.lineNumber = lineNumber;
    }
    public String toString() { // make it printable for debugging
        return "Token("+kind.toString()+" "+string+" "+lineNumber+")";
    }
}

//This is the Node for the Blocks on the Stack. It holds the Block Number and a ArrayList.
class block_node{

    public int block_number;
    ArrayList <Variable_Node> variable_storage;
    public block_node(int block_number){
        this.block_number = block_number;
        this.variable_storage = new ArrayList <Variable_Node>();
    }

}

//This is the Node in the Array List that has all the variables in that block.
//It stores the block number in use, the name of the variable name.

class Variable_Node{
    
    public String variable_name;
    public int block_number;

    public Variable_Node(String variable_name, int block_number){
        this.variable_name = variable_name;
        this.block_number = block_number;

    }

}