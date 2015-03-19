library(MASS)

Bootstrap <- function(X) function(f) function(n) {
  bs <- replicate(n, f(sample(X, length(X), replace = TRUE)))
  signif(c(estimate = mean(bs), error = sd(bs)), 3)
}

BootstrapDays <- Bootstrap(quine$Days)

ThetaA <- BootstrapDays(function(x) sd(x) / mean(x))
ThetaB <- BootstrapDays(function(x) median(x) / diff(range(x)))
ThetaC <- BootstrapDays(function(x) diff(range(x)) / sd(x))
