source('./preamble.R')

prob_diff <- function(n, p, whichWay, x) {
  # Since `pbinom` and `pnorm` only work for P(X <= x) or P(X > x),
  # we have to use the properties of integers and inequalities.
  adj <- adjust_discrete(whichWay, x)
  true_prob <- pbinom(adj$x, n, p, adj$lower)
  approx_prob <- pnorm(adj$x, n * p, sqrt(n * p * (1 - p)), adj$lower)

  abs(true_prob - approx_prob)
}
