source("prob3.R")

ActualData <- as.numeric(lynx)

png("prob3b.png")
hist(ActualData, main = "The Population of Lynx Trappings")
dev.off()

ii  <- SampleResults(ActualData, 10, 1000,
    filename = "prob3b_ii.png", main = "Samples of Lynx Trappings (n = 10)"
)
iii <- SampleResults(ActualData, 30, 1000,
    filename = "prob3b_iii.png", main = "Samples of Lynx Trappings (n = 30)"
)
iv  <- SampleResults(ActualData, 50, 1000,
    filename = "prob3b_iv.png", main = "Samples of Lynx Trappings (n = 50)"
)
v   <- SampleResults(ActualData, 100, 1000,
    filename = "prob3b_v.png", main = "Samples of Lynx Trappings (n = 100)"
)

ii$mean
ii$sd

iii$mean
iii$sd

iv$mean
iv$sd

v$mean
v$sd
