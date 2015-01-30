# We want each coin to know its probabilities.
colors <- c("Red", "Blue", "Green")
heads <- c(0.4, 0.7, 0.5)
coins <- cbind(colors, heads)

# We choose one coin with equal probability.
# ChooseCoin : [Coin] -> Coin
ChooseCoin <- function(coins) {
  len <- length(coins) / 2
  i <- sample(len, 1)
  coins[i, ]
}

# Given some coin, we flip it and report whether it's "H" or "T",
# for `heads` and `tails` respectively.
# FlipCoin : Coin -> H | T
FlipCoin <- function(coin) {
  if (coin[2] > runif(1, 0, 1)) "H" else "T"
}

# Tells whether a coin is blue or not.
# IsBlue : Coin -> Boolean
IsBlue <- function(coin) {
  coin[1] == "Blue"
}

# Tests for whether event A would succeed.
# ProbabilityA : Coin -> Boolean
ProbabilityA <- function(coin) {
  IsBlue(coin)
}

# Tests for whether event B would succeed.
# ProbabilityB : Coin -> Boolean
ProbabilityB <- function(coin) {
  FlipCoin(coin) == "H"
}

# We simulate `n` runs of supplied predicate,
# returning only the number that pass the predicate.
# Simulate : (Deck -> Boolean, Int) -> Int
Simulate <- function(p, n) {
  filtered <- Filter(function(x) { p(ChooseCoin(coins)) }, 1:n)
  length(filtered)
}

# The actual probability runner.
# Probability : (Deck -> Boolean) -> {n : [Float], probability : [Float]}
Probability <- function(f) {
  sim.ns <- c(100, 1000, 10000, 100000)
  sims <- sapply(sim.ns, function(n) { Simulate(f, n) / n })

  data.frame(n = sim.ns, probability = sims)
}
