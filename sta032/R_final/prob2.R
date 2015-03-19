library(MASS)
X <- quine$Days

Bootstrap <- function(f) function(n) {
  bs <- replicate(n, f(sample(X, length(X), replace = TRUE)))
  signif(c(estimate = mean(bs), error = sd(bs)), 3)
}

ThetaA <- Bootstrap(function(x) sd(x) / mean(x))
ThetaB <- Bootstrap(function(x) median(x) / diff(range(x)))
ThetaC <- Bootstrap(function(x) diff(range(x)) / sd(x))
