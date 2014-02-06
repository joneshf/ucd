# nesting of do-in-if-in-do
var i k rav
i := 25
do i /= 10 ->
    if i < 20 ->
	k := 20 - i
	do k < 0 -> print k k:=k+1 od
    [] i < 15 ->
	k := 15 - i
	do k < 0 -> var long rav long := 10 * k print long k:=k+1 od
	print long
    else ->
	k := 10 - i
	do k < 0 -> print k k:=k+1 od
    fi
    i := i - 1
od
print i
print k

