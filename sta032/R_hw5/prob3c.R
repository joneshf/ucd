png('prob3c1.png')
plot(all.n, both[, 1], type = 'n', main = 'Difference without continuity correction',
  xlab = 'n', ylab = 'difference', ylim = c(0, 0.14))
lines(all.n, both[, 1])
dev.off()

png('prob3c2.png')
plot(all.n, both[, 2], type = 'n', main = 'Difference with continuity correction',
  xlab = 'n', ylab = 'difference', ylim = c(0, 0.0012))
lines(all.n, both[, 2])
dev.off()
