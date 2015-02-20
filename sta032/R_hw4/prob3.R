SampleMean <- function(X, n) {
  mean(sample(X, n))
}

SampleN <- function(X, n, N) {
  sapply(c(1:N), function(N) { SampleMean(X, n) })
}

SampleResults <- function(X, n, N, filename = "", main = "Samples") {
  samples <- SampleN(X, n, N)

  png(filename)
  hist(samples, main = main)
  dev.off()

  list(mean = mean(samples), sd = sd(samples))
}
