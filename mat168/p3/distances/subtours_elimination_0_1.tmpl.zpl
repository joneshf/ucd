# Beginning of a TSP model

# Filename
param name := PUT_THE_FILENAME_HERE;

# Number of cities
param n := read name as "1n" use 1;

set V := { 1..n };

set E := { <i, j> in V cross V with i < j };

# Edge variables
var x[E] binary;

# Distances
param d[E] := read name as "<1n, 2n> 3n" skip 1;

minimize tour_length:
  sum <i,j> in E : d[i,j] * x[i,j];

subto degree:
  forall <v> in V do
    sum <v,j> in E : x[v,j]  +  sum <i,v> in E : x[i,v] == 2;

# Subtour elimination:

set S[] := powerset(V);

set S_Indices := indexset(S);

subto no_subtour:

  forall <s_index> in S_Indices with
    card(S[s_index]) >= 3 and card(S[s_index]) <= n - 3 do

      sum <i,j> in E with <i> in S[s_index] and <j> in S[s_index] :
        x[i,j] <= card(S[s_index]) - 1;
