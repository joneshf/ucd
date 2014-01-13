# test fa
var i j rav
fa i := 1 to 4 -> print i af

fa i := 2 to 5 ->
	fa j := i to 5 ->
		print i*100+j
	af
af

fa i := 2 to 9 ->
	fa j := 1 to 5 st i /= j ->
		print i*100+j
	af
af

fa j := 10 to 3 ->
	print 2000+j
af
