# test order of variable reference report
var a i k rav
i := 25
if i < 20 -> var a b rav print a else -> var a c rav print a+c fi
if i > 20 -> var a b rav print a else -> var a c rav print a+c fi
if i < 20 -> var a b rav print a [] i < 30 -> var a d e rav print a+d+e else -> var a c rav print a+c fi

if i < 20 -> if 1 -> var a b rav print a fi [] i < 30 -> if 2 -> var a d e rav print a+d+e fi else -> if 3 -> var a c rav print a+c fi fi

print i
