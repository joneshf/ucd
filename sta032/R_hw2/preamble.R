CardValue = rep(c("A", 2:10, "J", "Q", "K"), times = 4)
CardSuit  = rep(c("S", "C", "H", "D"),  each = 13)
CardDeck  = t(rbind(CardValue, CardSuit))
