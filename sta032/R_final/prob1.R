male <- "Male"
female <- "Female"
sexProbs <- c(0.65, 0.35)

protoss <- "Protoss"
terran <- "Terran"
zerg <- "Zerg"
races <- c(protoss, terran, zerg)
maleProbs <- c(0.3, 0.3, 0.4)
femaleProbs <- c(0.45, 0.35, 0.2)

Simulate <- function(n) signif(Raw(n), 3)

Raw <- function(n) {
  sex <- sample(c(male, female), n, prob = sexProbs, replace = TRUE)
  race <- sapply(c(1:n), function (x) ChooseRace(sex[x]))

  c( "P(Z)"          = sum(race == zerg) / n
   , "P(T)"          = sum(race == terran) / n
   , "P(P)"          = sum(race == protoss) / n
   , "P(M|Z)"        = sum(sex == male & race == zerg) / sum(race == zerg)
   , "P(M^c|P^c)"    = sum(sex != male & race != protoss) / sum(race != protoss)
   , "P(Z \\cap M^c)" = sum(race == zerg & sex != male) / n
   , "P(T \\cup M)"   = sum(race == terran | sex == male) / n
   )
}

ChooseRace <- function(sex)
  sample(races, 1, prob = if (sex == male) maleProbs else femaleProbs)
