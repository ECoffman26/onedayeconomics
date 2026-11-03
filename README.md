# Earthquake preparedness: break-even analysis

R code for *Economic assessment of an earthquake preparedness initiative* (Coffman, 2026).

How many lives must one civilian rescue unit save to pay for itself?

Interactive version: [link]

## Model

```
Cost        = Initial cost + maintenance × Annuity factor
Benefit     = VSL × (P20 / 20) × Annuity factor
Break-even  = Cost / Benefit
DF          = (1 − (1 + r)^−20) / r
```

Cost is the unit's initial cost plus 20 years of maintenance, in today's money. Benefit is the value of one life saved (VSL), weighted by the yearly chance of a major earthquake (P20 / 20) and discounted over 20 years (DF). Break-even is the number of lives a unit must save for benefit to equal cost.

Base case: initial cost NIS 260,000, maintenance NIS 80,000/year, r = 7%, P20 = 6%, VSL = NIS 9.7M → **3.6 lives**.

## Scripts

- `BE_sensitivity_prob.R`: break-even and return period for earthquake probabilities of 1–18%. Table and chart.
- `BE_sensitivity_VSL.R`: break-even for a VSL of NIS 8–20M. Table and chart.
- `BE_sensitivity_discount.R`: break-even for discount rates of 0–10%. Table and chart.
- `BE_2waysensitivity_prob_and_VSL.R`: break-even for every combination of probability (1–10%) and VSL (NIS 4–20M). Table.

Parameters are set at the top of each script.

## Requirements

R with `ggplot2`, `dplyr`, `scales`.
