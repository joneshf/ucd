X <- read.csv("STA-32-Winter-2015-Final-Data.txt")$X64

png("prob4.png")
hist(X)
dev.off()

NonParImpl <- function(b) function(mu0) {
  X.boot <- X - mean(X) + mu0
  bs <- replicate(b, mean(sample(X.boot, length(X.boot), replace = TRUE)))
  signif(mean(bs < mean(X)), 3)
}

NonPar <- NonParImpl(100000)

ParImpl <- function(b) function(mu0) {
  s <- sd(X)
  bs <- replicate(b, mean(rnorm(X, mu0, s)))
  signif(mean(bs < mean(X)), 3)
}

Par <- ParImpl(100000)

Theor <- function(mu0) {
  X.bar <- mean(X)
  s <- sd(X)
  n.root <- sqrt(length(X))
  z <- (X.bar - mu0) / (s / n.root)
  signif(pnorm(z), 3)
}
