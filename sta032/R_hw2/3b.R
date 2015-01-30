source("./3.R")

# We simulate the `ProbabilityN` along with `BothDamage` function,
# as these are both valid conditions for the allocation sector to be damaged.
probability.n <- Probability(function(drive) {
  ProbabilityN(drive) || BothDamage(drive)
})
