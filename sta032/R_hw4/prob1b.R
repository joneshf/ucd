Z <- (NormalData - mean(NormalData)) / sd(NormalData)

png("prob1b.png")
hist(Z, main = "Sandardized Dataset", xlim = c(-5, 5), ylim = c(0, 25))
dev.off()

mean(Z)

sd(Z)
