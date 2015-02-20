Z <- (LynxData - mean(LynxData)) / sd(LynxData)

png("prob2b.png")
hist(Z, main = "Sandardized Dataset", xlim = c(-5, 5), ylim = c(0, 100))
dev.off()

mean(Z)

sd(Z)
