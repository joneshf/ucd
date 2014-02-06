# test scoping. i.e., that correct a is used.
var a rav
a := 999
print a+a
if a<0 -> print 1111 else -> var a rav a:=8888 print a fi
print a

if a<0 -> print 1111
else ->
    print 2222
    print a
    if 0-2 -> var a rav a := 3 print a fi
    print a
fi
print a

if a<0-> print 1111
else ->
    var a rav
    a := 4
    print 5
    print a
    if 0 -> var a rav a := 6 print a fi
    print a
fi
print a
