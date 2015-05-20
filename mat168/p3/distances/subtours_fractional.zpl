# Beginning of a TSP model

# Filename
param name := "ulysses22Distances.txt";

# Number of cities
param n := read name as "1n" use 1;

set V := { 1..n };

set E := { <i, j> in V cross V with i < j };

# Edge variables
var x[E] real <= 1;

# Distances
param d[E] := read name as "<1n, 2n> 3n" skip 1;

minimize tour_length:
  sum <i,j> in E : d[i,j] * x[i,j];

subto degree:
  forall <v> in V do
    sum <v,j> in E : x[v,j]  +  sum <i,v> in E : x[i,v] == 2;
