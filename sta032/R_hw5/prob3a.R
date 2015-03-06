all.n <- seq(10, 400, 2)
p <- 0.50
whichWay <- '<='
both <- prob_both(all.n, p, whichWay)
colMeans(both)
