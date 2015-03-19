Corrected <- function(trials, alpha) {
  X <- sum(trials)
  n <- length(trials)
  n.tilde <- n + 4
  p.tilde <- (X + 2) / n.tilde
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  foo <- sqrt(p.tilde * (1 - p.tilde) / n.tilde)
  bar <- z * foo
  signif(c(low = p.tilde - bar, high = p.tilde + bar), 3)
}

Uncorrected <- function(trials, alpha) {
  X <- sum(trials)
  n <- length(trials)
  p.hat <- X / n
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  foo <- sqrt(p.hat * (1 - p.hat) / n)
  bar <- z * foo
  signif(c(low = p.hat - bar, high = p.hat + bar), 3)
}

Confidences <- function(trials, alpha) Conf(alpha)(trials)

Conf <- function(alpha) function(trials)
  matrix( c(Corrected(trials, alpha), Uncorrected(trials, alpha)), 2, 2, TRUE
        , list(c("Corrected", "Uncorrected"), c("Lower", "Upper")))

Proportion <- function(pop, alpha, size, simulations, true.mean) {
  # This is a `size x simulations` matrix.
  sims <- replicate(simulations, sample(pop, size))

  # Construct a confidence interval for each simulation.
  # This is a `4 x simulations` matrix
  confs <- apply(sims, 2, Conf(alpha))
  # Grab row vectors of each respective low and high in the interval.
  # Each is a `1 x simulations` row vector.
  corrected.low    <- confs[1, ]
  corrected.high   <- confs[3, ]
  uncorrected.low  <- confs[2, ]
  uncorrected.high <- confs[4, ]

  covered.mean <- covered(true.mean)

  # Find out how many intervals covered the true.mean.
  # These are both `1 x simulations` row vectors.
  corrected.covered <- mapply(covered.mean, corrected.low, corrected.high)
  uncorrected.covered <- mapply(covered.mean, uncorrected.low, uncorrected.high)

  # Using the duck typing of R, take the mean of both covered vectors.
  c(mean(corrected.covered), mean(uncorrected.covered))
}

# Helper function that curries its arguments.
covered <- function(val) function(low, high)
    low <= val && val <= high
