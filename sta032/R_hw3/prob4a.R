GeometricRv <- function(p) {
  # We just recursively count the number of iterations.
  # The function is pure, and we only need to worry about recursion depth.
  # However, since we're using decent sized probabilities,
  # we shouldn't hit the limit.
  go <- function(count) {
    if (runif(1) < p) count else go(count + 1)
  }

  # Start it off.
  go(1)
}
