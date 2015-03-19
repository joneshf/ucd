source("../R_final/prob3.R")

Coverages <- function(alpha, n, N) function(p) {
  sims <- replicate(N, rbinom(n, 1, p))
  confs <- apply(sims, 2, Conf(alpha))

  corrected.low    <- confs[1, ]
  corrected.high   <- confs[3, ]
  uncorrected.low  <- confs[2, ]
  uncorrected.high <- confs[4, ]

  covered.mean <- covered(p)

  corrected.covered <- mapply(covered.mean, corrected.low, corrected.high)
  uncorrected.covered <- mapply(covered.mean, uncorrected.low, uncorrected.high)

  c(mean(corrected.covered), mean(uncorrected.covered))
}

MedianM <- function(alpha, n, N, M) function(p) {
  sims <- replicate(M, Coverages(alpha, n, N)(p))
  c(median(sims[1, ]), median(sims[2, ]))
}

ManyP <- function(alpha, n, N, M, many.p) {
  sims <- sapply(many.p, MedianM(alpha, n, N, M))
  matrix(c(sims[1, ], sims[2, ]), length(many.p), 2)
}

x <- ManyP(0.05, 40, 200, 100, seq(0, 1, 0.05))

png("prob1a.png")

matplot(seq(0, 1, 0.05), x, type=c("l"), col = 1:2, xlab = "Probability",
        ylab = "Coverage")
title("Coverage for n = 40")
legend("bottom", legend = c("Corrected", "Uncorrected"), lty=1:2, col = 1:2)

dev.off()

x <- ManyP(0.05, 80, 200, 100, seq(0, 1, 0.05))

png("prob1b.png")

matplot(seq(0, 1, 0.05), x, type=c("l"), col = 1:2, xlab = "Probability",
        ylab = "Coverage")
title("Coverage for n = 80")
legend("bottom", legend = c("Corrected", "Uncorrected"), lty=1:2, col = 1:2)

dev.off()

x <- ManyP(0.05, 120, 200, 100, seq(0, 1, 0.05))

png("prob1c.png")

matplot(seq(0, 1, 0.05), x, type=c("l"), col = 1:2, xlab = "Probability",
        ylab = "Coverage")
title("Coverage for n = 120")
legend("bottom", legend = c("Corrected", "Uncorrected"), lty=1:2, col = 1:2)

dev.off()
