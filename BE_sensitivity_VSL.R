# Sensitivity analysis - VSL

library(ggplot2)
library(dplyr)
library(scales)

C_unit    <- 260000      # upfront cost per unit (NIS)
A         <- 80000       # annual sustainment cost per unit (NIS, real)
n         <- 20          # timeframe (years)
P20       <- 0.06      # probability of earthquake within 20 years
p         <- P20 / n     # annual hazard (uniform)
r         <- 0.07
annuity   <- (1 - (1 + r)^(-n)) / r

cost_npv <- function (C_unit, A, annuity) {
  C_unit + A * annuity
}

benefit_per_life <- function (p, annuity, VSL) {
  p * annuity * VSL
}

breakeven_lives <- function(C_unit, A, annuity, p, VSL) {
  cost_npv(C_unit, A, annuity) / benefit_per_life(p, annuity, VSL)
}


VSL <- sort(c(seq(8000000, 20000000, by = 1000000), 9700000))


results <- data.frame(VSL) %>%
  mutate(
    cost_npv         = cost_npv(C_unit, A, annuity),
    benefit_per_life = benefit_per_life(p, annuity, VSL),
    BE_lives         = cost_npv / benefit_per_life,
  )

print(results %>%
        mutate(across(c(cost_npv, benefit_per_life), ~ round(.x, 0)),
               BE_lives = round(BE_lives, 1))
)



curve_df <- data.frame(VSL = seq(5000000, 20000000, by = 1000000)) %>%
  mutate(BE_lives = breakeven_lives(C_unit, A, annuity, p, VSL))


p_main <- ggplot(curve_df, aes(x = VSL, y = BE_lives)) +
  geom_line(linewidth = 1.1, colour = "#1f4e79") +
  geom_point(data = results, aes(x = VSL, y = BE_lives),
             size = 2.6, colour = "#1f4e79") +
  geom_vline(xintercept = 9700000, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 9700000, y = 6,
           label = "9.7M — assumption used in this report",
           hjust = -0.05, size = 3.2, colour = "grey30") +
  scale_x_continuous(
    breaks = seq(4e6, 20e6, by = 2e6),
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(xlim = c(4e6, 20e6)) +
  scale_y_continuous(limits = c(0, max(curve_df$BE_lives) * 1.15),
                     breaks = seq(0, 8, 1)) +
  labs(
    title    = "Breakeven for Value of Statistical Life",
    subtitle = paste0("Lives one unit must save for break-even.  ",
                      "P = 6%, r = 7%"),
    x = "VSL",
    y = "Breakeven lives per unit"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(colour = "grey30", size = 9.5),
    panel.grid.minor = element_blank()
  )

print(p_main)
ggsave("appendix_b_breakeven_vs_VSL.png", p_main,
       width = 7.5, height = 4.8, dpi = 300)

results_out <- results %>%
  filter(VSL >= 8e6, VSL <= 20e6, VSL %% 2e6 == 0 |VSL == 9700000) %>%
  mutate(
    VSL              = scales::comma(VSL, accuracy = 1, big.mark = ","),
    cost_npv         = format(round(cost_npv, 0), big.mark = ","),
    benefit_per_life = format(round(benefit_per_life, 0), big.mark = ","),
    BE_lives         = round(BE_lives, 1)
  ) %>%
  rename(
    `Cost NPV (NIS)`           = cost_npv,
    `Benefit per life (NIS)`   = benefit_per_life,
    `Breakeven lives per unit` = BE_lives
  )

print(results_out)


