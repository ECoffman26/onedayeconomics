
# Sensitivity analysis - Probability

library(ggplot2)
library(dplyr)
library(scales)

C_unit    <- 260000      # upfront cost per unit (NIS)
A         <- 80000       # annual sustainment cost per unit (NIS, real)
n         <- 20          # timeframe (years)
VSL       <- 9700000     # value of a statistical life (NIS)
r         <- 0.07
annuity   <- (1 - (1 + r)^(-n)) / r

cost_npv <- function (C_unit, A, annuity) {
  C_unit + A * annuity
  }

benefit_per_life <- function(p20, n, annuity, VSL) {
  (p20/n) * annuity * VSL
}

breakeven_lives <- function(C_unit, A, annuity, p20, n, VSL) {
  cost_npv(C_unit, A, annuity) / benefit_per_life(p20, n, annuity, VSL)
}


probability <- c(seq(0.01, 0.18, by = 0.01))

return_period <- function(p20, n) {
  1 / (1 - (1 - p20)^(1 / n))
}


results <- data.frame(p20 = probability) %>%
  mutate(
    return_period    = return_period(p20, n),
    cost_npv         = cost_npv(C_unit, A, annuity),
    benefit_per_life = benefit_per_life(p20, n, annuity, VSL),
    BE_lives         = cost_npv / benefit_per_life,
  )

print(results %>%
    mutate(across(c(cost_npv, benefit_per_life, return_period), ~ round(.x, 0)),
           BE_lives = round(BE_lives, 1))
  )



curve_df <- data.frame(p20 = seq(0.01, 0.18, by = 0.001)) %>%
  mutate(BE_lives = breakeven_lives(C_unit, A, annuity, p20, n, VSL))


p_main <- ggplot(curve_df, aes(x = p20, y = BE_lives)) +
  geom_line(linewidth = 1.1, colour = "#1f4e79") +
  geom_point(data = results, aes(x = p20, y = BE_lives),
             size = 2.6, colour = "#1f4e79") +
  geom_vline(xintercept = 0.06, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.06, y = 20,
           label = "6% — assumption used in this report",
           hjust = -0.05, size = 3.2, colour = "grey30") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = probability) +
  scale_y_continuous(limits = c(0, max(curve_df$BE_lives) * 1.15),
                     breaks = seq(0, 25, 5)) +
  labs(
    title    = "The breakeven falls as hazard probability rises",
    subtitle = paste0("Lives one unit must save for break-even.  ",
                      "VSL = NIS ", format(VSL, big.mark = ",")),
    x = "Probability of a major earthquake within 20 years",
    y = "Breakeven lives per unit"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey30", size = 9.5),
    panel.grid.minor = element_blank()
  )

print(p_main)
ggsave("appendix_b_breakeven_vs_hazard.png", p_main,
       width = 7.5, height = 4.8, dpi = 300)

results_out <- results %>%
  mutate(
    p20              = percent(p20),
    return_period    = format(round(return_period, 0), big.mark = ","),
    cost_npv         = format(round(cost_npv, 0), big.mark = ","),
    benefit_per_life = format(round(benefit_per_life, 0), big.mark = ","),
    BE_lives         = round(BE_lives, 1)
  ) %>%
  rename(
    `Probability`              = p20,
    `Return Period (years)`    = return_period,
    `Cost NPV (NIS)`           = cost_npv,
    `Benefit per life (NIS)`   = benefit_per_life,
    `Breakeven lives per unit` = BE_lives
  )

