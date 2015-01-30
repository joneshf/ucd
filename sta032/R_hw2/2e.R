source("./2.R")

# Since these two events are not independent,
# we have to convert to logic and test it.
# Logically this is A /\ B
probability.a.cap.b <- Probability(function(coin) {
  ProbabilityA(coin) && ProbabilityB(coin)
})
