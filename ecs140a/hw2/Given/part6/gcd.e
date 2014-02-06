var x y rav
x := 12
y := 21
do x < y -> print y y := y - x
[] x > y -> print x x := x - y
od
print x
print y
