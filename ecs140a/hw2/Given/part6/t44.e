# test order of variable reference report
var a i k rav
i := 25
if i < 20 ->
  # no vars here
  if i < 10 ->
    if i < 0 ->
      var i rav
      i := 888
      print i
    fi
  fi
else ->
  var i rav
  print i
  if i < 0 ->
    var i rav
    i := 999
    print i
  fi
  print i
fi
