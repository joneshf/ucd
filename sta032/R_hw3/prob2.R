source("./prob1.R")

BinomialProb2 <- function(n, p, x) {
  # We can use `BinomialProb` to just `sapply` each element of `x`.
  # This is a case where the curried version would make things clearer.
  # We could say:
  # sapply(x, BinomialProb(n)(p))

  sapply(x, function(x1) { BinomialProb(n, p, x1) })
}
