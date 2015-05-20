# Beginning of a TSP model

# Edge variables
var x12 binary;
var x13 binary;
var x14 binary;
var x15 binary;
var x16 binary;
var x23 binary;
var x24 binary;
var x25 binary;
var x26 binary;
var x34 binary;
var x35 binary;
var x36 binary;
var x45 binary;
var x46 binary;
var x56 binary;

minimize tour_length:
  10 * x12 + 10 * x13 + 55 * x14 + 55 * x15 + 55 * x16
           + 10 * x23 + 55 * x24 + 55 * x25 + 55 * x26
	     	      + 33 * x34 + 44 * x35 + 55 * x36
		      	     	 + 10 * x45 + 10 * x46
				      	    + 10 * x56;

subto degree_node_1:
  x12 + x13 + x14 + x15 + x16 == 2;

subto degree_node_2:
  x12 + x23 + x24 + x25 + x26 == 2;

subto degree_node_3:
  x13 + x23 + x34 + x35 + x36 == 2;

subto degree_node_4:
  x14 + x24 + x34 + x45 + x46 == 2;

subto degree_node_5:
  x15 + x25 + x35 + x45 + x56 == 2;



