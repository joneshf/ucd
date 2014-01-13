# test parser -- this is another valid program

var
	a b another i k s
rav
a:= 100 b := 200
print a print b
a := 44
b := 0-a a := 888-999 another := a - 999
print a print b print another
# how's precedence, associativity?
print  3+2*4	print (8 - 7 - 6)	print (8-(7-6))	print ((8-7)-6)

i := 1
do i<= 10 -> print i i:=i+1 od

i := 20
if i=10 -> i:=15 [] i=20 -> i:=5 fi
print i

i := 10
if i=10 -> i:=15 [] i=20 -> i:=5 fi
print i

i := 40
if i=10 -> i:=15 [] i=20 -> i:=5 fi
print i

s:=0k:=10
do k>0 -> s:=s+k k:= k-1 od
print k print s
