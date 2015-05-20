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

# Distances
param d12 := 10;
param d13 := 10;
param d14 := 55;
param d15 := 55;
param d16 := 55;
param d23 := 10;
param d24 := 55;
param d25 := 25;
param d26 := 55;
param d34 := 33;
param d35 := 44;
param d36 := 55;
param d45 := 10;
param d46 := 10;
param d56 := 10;

minimize tour_length:
  d12 * x12 + d13 * x13 + d14 * x14 + d15 * x15 + d16 * x16
            + d23 * x23 + d24 * x24 + d25 * x25 + d26 * x26
	     	        + d34 * x34 + d35 * x35 + d36 * x36
		      	     	    + d45 * x45 + d46 * x46
				      	        + d56 * x56;

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



