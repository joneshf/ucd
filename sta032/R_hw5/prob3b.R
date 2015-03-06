png('prob3b1.png')
hist(both[, 1], main = 'Difference without continuity correction',
  xlab = 'difference', xlim = c(0, 0.14), ylim = c(0, 120))
dev.off()

png('prob3b2.png')
hist(both[, 2], main = 'Difference with continuity correction',
  xlab = 'difference', xlim = c(0, 0.0012), ylim = c(0, 200))
dev.off()
