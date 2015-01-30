source("./preamble.R")
source("./1.R")

# We want a function that checks if a card is a heart.
# IsHeart : Card -> Boolean
IsHeart <- function(card) {
  card[2] == "H"
}

# We want a function that checks if two cards are hearts.
# AreHearts : (Card, Card) -> Boolean
AreHearts <- function(card1, card2) {
  IsHeart(card1) && IsHeart(card2)
}

# We draw two cards and check if they're both hearts.
# DrawHearts : Deck -> Boolean
DrawHearts <- function(deck) {
  drawn <- Draw2(deck)
  AreHearts(drawn$card1, drawn$card2)
}

# We actually run the simulation and return a data frame with probabilities.
probability.hearts <- Probability(DrawHearts)
