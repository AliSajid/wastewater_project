# Process the incidence data

library(tidyverse)
library(lubridate)

file <- "raw/COVIDDeathData_CountyOfResidence.csv"

col_spec <- cols(
  County = col_character(),
  Sex = col_factor(),
  `Age Range` = col_factor(),
  `Onset Date` = col_date(format = "%Y-%m-%d"),
  `Admission Date` = col_date(format = "%Y-%m-%d"),
  `Date Of Death` = col_date(format = "%Y-%m-%d"),
  `Case Count` = col_double(),
  `Hospitalized Count` = col_double(),
  `Death Due To Illness Count - County Of Residence` = col_double()
)

dataset <- read_csv(file, col_types = col_spec, na = c("", " ", "Unknown")) |>
  filter(County == "Lucas") |>
  select(-Sex, -`Age Range`, -`Admission Date`, -`Date Of Death`, -`Hospitalized Count`, -`Death Due To Illness Count - County Of Residence`) |>
  group_by(`Onset Date`) |>
  summarise(cases = sum(`Case Count`)) |>
  rename(date = `Onset Date`) |>
  filter(date >= ymd("2021-01-14")) |>
  write_csv("data/case_counts_lucas.csv")

