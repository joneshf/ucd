source("./preamble.R")
source("./1.R")
source("./1a.R")
source("./1b.R")

# We want a function that checks if a card is a diamond.
# IsDiamond : Card -> Boolean
IsDiamond <- function(card) {
  card[2] == "D"
}

# We want to ensure one card is a heart and the other a diamond.
AreHeartDiamond <- function(card1, card2) {
  (IsHeart(card1) && IsDiamond(card2)) || (IsDiamond(card1) && IsHeart(card2))
}

# We reuse our functions from before
AreComplex <- function(card1, card2) {
  ArePair(card1, card2) && AreHeartDiamond(card1, card2)
}

# We draw two cards and check if they're a pair
# with one a heart and the other a diamond.
# DrawComplex : Deck -> Boolean
DrawComplex <- function(deck) {
  drawn <- Draw2(deck)
  AreComplex(drawn$card1, drawn$card2)
}

# We actually run the simulation and return a data frame with probabilities.
probability.complex <- Probability(DrawComplex)
