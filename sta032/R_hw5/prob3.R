source('./prob1.R')
source('./prob2.R')

prob_both <- function(ns, p, op) {
  no_cor   <- sapply(ns, function(n) prob_diff(n, p, op, n / 2))
  with_cor <- sapply(ns, function(n) prob_diff_corrected(n, p, op, n / 2))
  matrix(c(no_cor, with_cor), nrow = length(ns), ncol = 2)
}
