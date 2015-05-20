# Beginning of a TSP model

# Number of cities
param n := 6;

set V := { 1..n };

set E := { <1, 2>, <1, 3>, <1, 4>, <1, 5>, <1, 6>,
                   <2, 3>, <2, 4>, <2, 5>, <2, 6>, 
                           <3, 4>, <3, 5>, <3, 6>, 
                                   <4, 5>, <4, 6>, 
                                           <5, 6> };

# Edge variables
var x[E] binary;

# Distances
param d[E] := 
  <1, 2>  10,
  <1, 3>  10,
  <1, 4>  55,
  <1, 5>  55,
  <1, 6>  55,
  <2, 3>  10,
  <2, 4>  55,
  <2, 5>  25,
  <2, 6>  55,
  <3, 4>  33,
  <3, 5>  44,
  <3, 6>  55,
  <4, 5>  10,
  <4, 6>  10,
  <5, 6>  10;

minimize tour_length:
  sum <i,j> in E : d[i,j] * x[i,j];

subto degree:
  forall <v> in V do
    sum <v,j> in E : x[v,j]  +  sum <i,v> in E : x[i,v] == 2;
