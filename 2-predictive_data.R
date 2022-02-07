# Create moving averages

library(tidyverse)
library(lubridate)
library(depmixS4)

case_data <- read_csv("data/case_counts_lucas.csv")

count_data <- read_csv("data/site_1.csv") |>
  pivot_longer(where(is.numeric), names_to = "source", values_to = "counts") |>
  separate(source, into = c("type", "replicate")) |>
  group_by(date, type) |>
  summarise(counts = mean(counts, na.rm = TRUE)) |>
  pivot_wider(names_from = type, values_from = counts) |>
  ungroup()


combined_data <- count_data |>
  inner_join(case_data) |>
  mutate(
    across(where(is.numeric), ~ if_else(is.nan(.x), NA_real_, .x)),
    scaled_n2 = scale(n2),
    scaled_cra = scale(cra),
    scaled_cases = scale(cases),
    n2_change = round((scaled_n2 - lag(scaled_n2)) / scaled_n2 * 100, 4),
    case_change = round((scaled_cases - lag(scaled_cases)) / scaled_cases * 100, 4)
  ) |>
  dplyr::select(date, n2_change, case_change) |>
  na.omit()


coded_data <- combined_data |>
  mutate(
    n2 = as_factor(if_else(n2_change > lag(n2_change), "up", "down")),
    cases = as_factor(if_else(case_change > lag(case_change), "up", "down"))
  ) |>
  na.omit() |>
  dplyr::select(date, n2, cases)


hmm <- depmix(case_change ~ n2_change, nstates = 4, data = combined_data)

mod <- fit(hmm)

probs <- posterior(mod, type = "viterbi")
