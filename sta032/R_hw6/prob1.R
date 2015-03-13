confidence <- function(x, alpha) {
  n <- length(x)
  s <- sd(x)
  x.bar <- mean(x)
  z <- qnorm(alpha / 2, lower.tail = FALSE)
  t <- qt(alpha / 2, n - 1, lower.tail = FALSE)
  s.root.n <- s / sqrt(n)
  z.sd <- z * s.root.n
  t.sd <- t * s.root.n
  vals <- c(x.bar - z.sd, x.bar - t.sd, x.bar + z.sd, x.bar + t.sd)

  matrix(vals, 2, 2)
}
