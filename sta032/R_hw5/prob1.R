prob_diff <- function(n, p, whichWay, x) {
  # Since `pbinom` and `pnorm` only work for P(X <= x) or P(X > x),
  # we have to use the properties of integers and inequalities.
  adj <- adjust(whichWay, x)
  true_prob <- pbinom(adj$x, n, p, adj$lower)
  approx_prob <- pnorm(adj$x, n * p, n * p * (1 - p), adj$lower)

  abs(true_prob - approx_prob)
}

adjust <- function(op, x) {
  switch( op
        , '<'  = list(x = x - 1, lower = TRUE)
        , '<=' = list(x = x,     lower = TRUE)
        , '>'  = list(x = x,     lower = FALSE)
        , '>=' = list(x = x - 1, lower = FALSE)
        )
}
