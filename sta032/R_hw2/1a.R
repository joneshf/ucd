source("./preamble.R")
source("./1.R")

# We want a function that checks if two cards are pairs.
# ArePair : (Card, Card) -> Boolean
ArePair <- function(card1, card2) {
  card1[1] == card2[1]
}

# We draw two cards and check if they're a pair.
# DrawPair : Deck -> Boolean
DrawPair <- function(deck) {
  drawn <- Draw2(deck)
  ArePair(drawn$card1, drawn$card2)
}

# We actually run the simulation and return a data frame with probabilities.
probability.pair <- Probability(DrawPair)
