# Sensitivity analysis - two-way: Probability x VSL
# Breakeven lives per unit for every combination of 20-year hazard
# probability (rows) and value of a statistical life (columns).

library(dplyr)
library(scales)

C_unit    <- 260000      # upfront cost per unit (NIS)
A         <- 80000       # annual sustainment cost per unit (NIS, real)
n         <- 20          # timeframe (years)
r         <- 0.07
annuity   <- (1 - (1 + r)^(-n)) / r

# Base-case assumptions (marked on the heatmap)
P20_base  <- 0.06
VSL_base  <- 9700000

cost_npv <- function(C_unit, A, annuity) {
  C_unit + A * annuity
}

benefit_per_life <- function(p20, n, annuity, VSL) {
  (p20 / n) * annuity * VSL
}

breakeven_lives <- function(C_unit, A, annuity, p20, n, VSL) {
  cost_npv(C_unit, A, annuity) / benefit_per_life(p20, n, annuity, VSL)
}

# ---- Grid values ------------------------------------------------------------

probability <- seq(0.01, 0.10, by = 0.01)
VSL_values  <- sort(c(seq(4e6, 20e6, by = 2e6), VSL_base))

# ---- Two-way table ----------------------------------------------------------

BE_matrix <- outer(probability, VSL_values, function(p20, VSL)
  breakeven_lives(C_unit, A, annuity, p20, n, VSL))

dimnames(BE_matrix) <- list(
  Probability = percent(probability, accuracy = 1),
  VSL         = paste0(VSL_values / 1e6, "M")
)

print(round(BE_matrix, 1))

# ---- Export -----------------------------------------------------------------

results_out <- data.frame(
  Probability = percent(probability, accuracy = 1),
  round(BE_matrix, 1),
  check.names = FALSE
)

print(results_out)




