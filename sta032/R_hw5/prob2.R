source('./preamble.R')

prob_diff_corrected <- function(n, p, whichWay, x) {
  # Since `pbinom` and `pnorm` only work for P(X <= x) or P(X > x),
  # we have to use the properties of integers and inequalities.
  adj <- adjust_discrete(whichWay, x)
  true_prob <- pbinom(adj$x, n, p, adj$lower)
  # We can add 0.5 to the adjusted `x` and get the proper `x`.
  approx_prob <- pnorm(adj$x + 0.5, n * p, sqrt(n * p * (1 - p)), adj$lower)

  abs(true_prob - approx_prob)
}
