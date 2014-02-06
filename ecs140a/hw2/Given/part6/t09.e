# nesting of do inside if in inside do
var i k long n rav
i := 25
do 10/=i ->
	if 20=i ->
		k := 20 - i
		do k -> print k k:=k+1 od
	[] 15=i ->
		k := 15 - i
		n:=5
		do k -> long:=10*k print long k:=k+1
		[] n -> print n n:=0
		od
		print long
	else ->
		k:=10-i
		do k -> print k k:= k+1 od
	fi
	i:=i-1
od
print i
