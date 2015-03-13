source("./prob1.R")

proportion <- function(pop, alpha, size, simulations, true.mean) {
  # This is a `size x simulations` matrix.
  sims <- replicate(simulations, sample(pop, size))

  # Construct a confidence interval for each simulation.
  # This is a `4 x simulations` matrix
  confs <- apply(sims, 2, function(sim) confidence(sim, alpha))
  # Grab row vectors of each respective low and high in the interval.
  # Each is a `1 x simulations` row vector.
  z.lows <- confs[1, ]
  z.highs <- confs[3, ]
  t.lows <- confs[2, ]
  t.highs <- confs[4, ]

  covered.mean <- covered(true.mean)

  # Find out how many intervals covered the true.mean.
  # These are both `1 x simulations` row vectors.
  z.covereds <- mapply(covered.mean, z.lows, z.highs)
  t.covereds <- mapply(covered.mean, t.lows, t.highs)

  # Using the duck typing of R, take the mean of both covered vectors.
  c(mean(z.covereds), mean(t.covereds))
}

# Helper function that curries its arguments.
covered <- function(val)
  function(low, high)
    low <= val && val <= high
