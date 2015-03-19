source("../R_final/prob3.R")

Bar <- function(alpha, n, N) function(p) {
  samples <- rbinom(n, 1, p)
  Proportion(samples, alpha, n, N, p)
}

Baz <- function(alpha, n, N) function(ps)
  sapply(ps, Bar(alpha, n, N))

Foo <- function(alpha, n, N, M, ps)
  replicate(M, Baz(ps))
