# Process the excel sheet to create tidy data per site per day

library(readxl)
library(tidyverse)
library(lubridate)

process_sheet <- function(file, sheet) {

  d <- read_excel(file, sheet)

  n2_col <- which(str_detect(names(d), "N2"))
  cra_col <- which(str_detect(names(d), "CrA"))
  last_col <- dim(d)[2]

  n2_length <- cra_col - n2_col
  cra_length <- last_col - cra_col + 1

  column_names <- c("date", str_c("n2", 1:n2_length, sep = "_"), str_c("cra", 1:cra_length, sep = "_"))

  timeseries <- d |>
    set_names(column_names) |>
    filter(!is.na(date)) |>
    mutate(date = if_else(date == "baseline", "1/14/21", date),
           date = str_replace(date, "--", "-"),
           date = str_replace_all(date, "/", "-"),
           date = str_replace_all(date, "2021", "21"),
           date = str_remove(date, "-R"),
           date = mdy(date)) |>
    mutate(across(-date, ~ as.numeric(str_remove_all(.x, "\\*"))),
           across(where(is.numeric), ~ round(.x, 4)))

  timeseries
}


raw_file <- "raw/UT-Cov2-Data.xlsx"

sheets <- excel_sheets(raw_file) |>
  keep(\(x) str_detect(x, "[sS]ite"))


dataset <- sheets |>
  map(~ process_sheet(raw_file, .x)) |>
  map2(1:5, ~ mutate(.x, site = str_c("site", .y, sep = "_"))) |>
  map2_dfr(1:5, ~ write_csv(.x, str_glue("data/site_{.y}.csv"))) |>
  write_csv("data/complete_wide.csv") |>
  pivot_longer(where(is.numeric), names_to = "source", values_to = "counts") |>
  separate(source, into = c("type", "replicate")) |>
  write_csv("data/complete_tidy.csv") |>
  group_by(date, site, type) |>
  summarise(counts = mean(counts, na.rm = TRUE)) |>
  write_csv("data/averaged_tidy.csv")
