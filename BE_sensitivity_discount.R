
# Sensitivity analysis - Discount rate

library(ggplot2)
library(dplyr)
library(scales)

C_unit    <- 260000      # upfront cost per unit (NIS)
A         <- 80000       # annual sustainment cost per unit (NIS, real)
n         <- 20          # timeframe (years)
P20       <- 0.06        # probability of earthquake within 20 years
p         <- P20 / n     # annual hazard (uniform)
VSL       <- 9700000     # value of a statistical life (NIS)


annuity <- function(r, n) {
  ifelse(r == 0, n, (1 - (1 + r)^(-n)) / r)
}

cost_npv <- function(r, C_unit, A, n) {
  C_unit + A * annuity(r, n)
}

benefit_per_life <- function(r, p, n, VSL) {
  p * annuity(r, n) * VSL
}

breakeven_lives <- function(r, C_unit, A, n, p, VSL) {
  cost_npv(r, C_unit, A, n) / benefit_per_life(r, p, n, VSL)
}


rates <- c(0, 0.03, 0.05, 0.07, 0.10)

results <- data.frame(r = rates) %>%
  mutate(
    annuity_factor   = annuity(r, n),
    cost_npv         = cost_npv(r, C_unit, A, n),
    p_star           = p * annuity_factor,          # prob-weighted discount factor
    benefit_per_life = p_star * VSL,
    BE_lives         = cost_npv / benefit_per_life,
  )

print(
  results %>%
    mutate(across(c(cost_npv, benefit_per_life), ~ round(.x, 0)),
           across(c(annuity_factor, BE_lives), ~ round(.x, 2)),
           p_star = round(p_star,5)),
)


curve_df <- data.frame(r = seq(0, 0.10, by = 0.001)) %>%
  mutate(BE_lives = breakeven_lives(r, C_unit, A, n, p, VSL))


p_main <- ggplot(curve_df, aes(x = r, y = BE_lives)) +
  geom_line(linewidth = 1.1, colour = "#1f4e79") +
  geom_point(data = results, aes(x = r, y = BE_lives),
             size = 2.6, colour = "#1f4e79") +
  geom_vline(xintercept = 0.07, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.07, y = 0.5,
           label = "7% - official rate (nohal PRAT)",
           hjust = 1.05, size = 3.2, colour = "grey30") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = rates) +
  scale_y_continuous(limits = c(0, max(curve_df$BE_lives) * 1.15),
                     breaks = seq(0, 14, 2)) +
  labs(
    title    = "The breakeven is insensitive to the discount rate",
    subtitle = "Lives one unit must save for Break-even;  VSL = 9,700,000;
              P(quake in 20 years) = 6%",
    x = "Real discount rate",
    y = "Breakeven lives per unit") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey30", size = 9.5),
    panel.grid.minor = element_blank()
  )

print(p_main)
ggsave("breakeven_vs_discount_rate.png", p_main,
       width = 7.5, height = 4.8, dpi = 300)

results_out <- results %>%
  mutate(
    r                = paste0(r * 100, "%"),
    annuity_factor   = round(annuity_factor, 3),
    cost_npv         = round(cost_npv, 0),
    p_star           = round(p_star, 5),
    benefit_per_life = round(benefit_per_life, 0),
    BE_lives         = round(BE_lives, 2)
  ) %>%
  rename(
    `Discount rate`            = r,
    `Annuity factor`           = annuity_factor,
    `Cost NPV (NIS)`           = cost_npv,
    `Prob-weighted factor`     = p_star,
    `Benefit per life (NIS)`   = benefit_per_life,
    `Breakeven lives per unit` = BE_lives
  )

print(results_out)