png("prob2a.png")
hist(LynxData, main = "Original Lynx Dataset", xlim = c(0, 7000), ylim = c(0, 100))
dev.off()

mean(LynxData)

sd(LynxData)
