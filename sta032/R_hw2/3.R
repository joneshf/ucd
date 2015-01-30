# We choose one drive with equal probability.
# ChooseDrive : () -> Both | Allocation | Nonessential | OK
ChooseDrive <- function() {
  d <- runif(1, 0, 1)
  if (d < 0.1) "Both"
  else if (d < 0.2) "Allocation"
  else if (d < 0.7) "Nonessential"
  else "OK"
}

# Tells whether a drive had allocation sector damage.
# AllocationDamage : Drive -> Boolean
AllocationDamage <- function(drive) {
  drive == "Allocation"
}

# Tells whether a drive had allocation and nonessential sector damage.
# BothDamage : Drive -> Boolean
BothDamage <- function(drive) {
  drive == "Both"
}

# Tells whether a drive had nonessential sector damage.
# NonessentialDamage : Drive -> Boolean
NonessentialDamage <- function(drive) {
  drive == "Nonessential"
}

# Let's call the probability that an allocation sector is damaged P(A)
# ProbabilityA : Drive -> Boolean
ProbabilityA <- function(drive) {
  AllocationDamage(drive)
}

# Let's call the probability that a nonessential sector is damaged P(N)
# ProbabilityN : Drive -> Boolean
ProbabilityN <- function(drive) {
  NonessentialDamage(drive)
}

# We simulate `n` runs of supplied predicate,
# returning only the number that pass the predicate.
# Simulate : (Deck -> Boolean, Int) -> Int
Simulate <- function(p, n) {
  filtered <- Filter(function(x) { p(ChooseDrive()) }, 1:n)
  length(filtered)
}

# The actual probability runner.
# Probability : (Deck -> Boolean) -> {n : [Float], probability : [Float]}
Probability <- function(f) {
  sim.ns <- c(100, 1000, 10000, 100000)
  sims <- sapply(sim.ns, function(n) { Simulate(f, n) / n })

  data.frame(n = sim.ns, probability = sims)
}
