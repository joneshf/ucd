source("./prob2.R")

BinomialWithinK <- function(n, p, k) {
  # Using the hint from the description, we calculate `mu` and `sigma`,
  # then use these in order to find the lower and higher bounds.
  # Next we compute the binomial probability with `BinomialProb2` and
  # `sum` it all up.

  mu    <- n * p
  sigma <- k * sqrt(mu * (1 - p))
  low   <- ceiling(mu - sigma)
  high  <- floor(mu + sigma)

  sum(BinomialProb2(n, p, (low:high)))
}
