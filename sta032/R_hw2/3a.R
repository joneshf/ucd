source("./3.R")

# We simulate the `ProbabilityA` along with `BothDamage` function,
# as these are both valid conditions for the allocation sector to be damaged.
probability.a <- Probability(function(drive) {
  ProbabilityA(drive) || BothDamage(drive)
})
