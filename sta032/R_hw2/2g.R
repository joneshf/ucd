source("./2.R")

# Since these two events are not independent,
# we have to convert to logic and test it.
# Logically this is ~B -> A
# which is equivalent to B \/ A
probability.a.given.b.comp <- Probability(function(coin) {
  ProbabilityB(coin) || ProbabilityA(coin)
})
