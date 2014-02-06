var a b c d rav

print ^5
print ^0-4
print (^5)+(^4)+(^3)+(^2)+(^1)+(^0)
print ^2
print ^^2
print ^^^2
print ^^^^2
print ^^^^^1
print ^^^^^^1
print ^^^^^^^1
print ^^^^^^^^1
print 3*^5-4+14

fa a := 0-10 to 10 ->
  print a
  print ^a
  print ^^a
af

print 11111111

print @5
print @0-4
print (@5)+(@4)+(@3)+(@2)+(@1)+(@0)
print @2
print @@2
print @@@2
print @@@@2
print @@@@@1
print @@@@@@1
print @@@@@@@1
print @@@@@@@@1
print 3*@5-4+14

fa a := 0-10 to 20 ->
  print a
  print @a
  print @@a
af

print 22222222

print @2222222

print ^@2222222

print 33333333
b := 0-55
c := 100*^b
d := @c
print b
print c
print d
d := 9
b := @^d
c := ^@d
print d
print b
print c

print 44444444
c := 2
d := 256
fa b := 1 to 4 ->
  c := 1000+(^c)-(999+1)
  print c
  d := 12*(@d)/(11+1)
  print d
af
