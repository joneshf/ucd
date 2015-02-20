source("prob3.R")

FakeData <- rnorm(1000, mean=5, sd=3)

png("prob3a.png")
hist(FakeData, main = "The Population")
dev.off()

ii  <- SampleResults(FakeData, 10, 1000,
    filename = "prob3a_ii.png", main = "Samples (n = 10)"
)
iii <- SampleResults(FakeData, 30, 1000,
    filename = "prob3a_iii.png", main = "Samples (n = 30)"
)
iv  <- SampleResults(FakeData, 50, 1000,
    filename = "prob3a_iv.png", main = "Samples (n = 50)"
)
v   <- SampleResults(FakeData, 100, 1000,
    filename = "prob3a_v.png", main = "Samples (n = 100)"
)

ii$mean
ii$sd

iii$mean
iii$sd

iv$mean
iv$sd

v$mean
v$sd
