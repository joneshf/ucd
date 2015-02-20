png("prob1a.png")
hist(NormalData, main = "Original Dataset", xlim = c(-5, 15), ylim = c(0, 25))
dev.off()

mean(NormalData)

sd(NormalData)
