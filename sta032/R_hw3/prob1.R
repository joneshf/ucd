BinomialProb <- function(n, p, x) {
  # We put the `n` and `p` arguments first
  # in an attempt to make things more composable.
  # Of course, it doesn't really matter as the function isn't curried.
  # If it were, it could provide usage such as
  # prob_n_p <- BinomialProb(n)(p)
  # prob_n_p_x <- foo(x)

  # The following law should hold
  # forall n, p, x. dbinom(x, n, p) == BinomialProb(n, p, x)
  # for some definition of `==`

  # Use a direct translation of the probability.
  choose(n, x) * (p ^ x) * ((1 - p) ^ (n - x))
}
