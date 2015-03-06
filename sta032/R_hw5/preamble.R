adjust_discrete <- function(op, x) {
  # Adjust for the discrete values used in `pbinom`.
  switch( op
        , '<'  = list(x = x - 1, lower = TRUE)
        , '<=' = list(x = x,     lower = TRUE)
        , '>'  = list(x = x,     lower = FALSE)
        , '>=' = list(x = x - 1, lower = FALSE)
        )
}
