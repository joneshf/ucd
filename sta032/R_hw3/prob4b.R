source("./prob4a.R")

Trials <- function(r, n, p) {
  # We simulate `n` runs by just calling `GeometricRv` and
  # comparing the output to `r` that many times.
  simulate <- sapply(1:n, function(x) {
    GeometricRv(p)
  })

  # Count how many are equal to `r`, and divide by `n`.
  # This is our probability.
  length(Filter(function(x) {x == r}, simulate)) / n
}
