source("./preamble.R")

# We want a function that takes a deck,
# draws one card at random,
# and returns both the deck after being drawn from, and the drawn card.
# Draw : Deck -> (Card, Deck)
Draw <- function(deck) {
  len <- length(deck) / 2
  i <- sample(len, 1)
  list(card = deck[i, ], deck = deck[-i, ])
}

# Using `Draw`, we draw two cards.
# Draw2 : Deck -> (Card1, Card2, Deck)
Draw2 <- function(deck) {
  drawn1 <- Draw(deck)
  drawn2 <- Draw(drawn1$deck)
  list(card1 = drawn1$card, card2 = drawn2$card, deck = drawn2$deck)
}

# We simulate `n` runs of supplied predicate,
# returning only the number that pass the predicate.
# Simulate : (Deck -> Boolean, Int) -> Int
Simulate <- function(p, n) {
  filtered <- Filter(function(x) { p(CardDeck) }, 1:n)
  length(filtered)
}

# The actual probability runner.
# Probability : (Deck -> Boolean) -> {n : [Float], probability : [Float]}
Probability <- function(f) {
  sim.ns <- c(100, 1000, 10000, 100000)
  sims <- sapply(sim.ns, function(n) { Simulate(f, n) / n })

  data.frame(n = sim.ns, probability = sims)
}
