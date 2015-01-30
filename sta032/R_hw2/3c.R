source("./3.R")

# We simulate the `ProbabilityN` along with `BothDamage` function,
# as these are both valid conditions for the allocation sector to be damaged.
# Logically this is A /\ N
probability.a.cap.N <- Probability(function(drive) {
  ProbabilityA(drive) && ProbabilityN(drive) || BothDamage(drive)
})
