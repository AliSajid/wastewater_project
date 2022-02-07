# Load the data and visualize basics


library(tidyverse)
library(lubridate)

col_spec <- cols(
  date = col_date(format = "%F"),
  site = col_factor(levels = str_c("site", 1:5, sep = "_")),
  type = col_factor(levels = c("n2", "cra")),
  counts = col_double()
)

dataset <-
  read_csv("data/averaged_tidy.csv", col_types = col_spec) |>
  mutate(lcounts = log10(counts))

g <- ggplot(dataset, aes(x = date, y = lcounts, color = type))

p <- g +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(site),
             labeller = labeller(site = ~ str_remove(str_to_title(.x), "_"))) +
  theme_minimal() +
  xlab("Date") + ylab("Log_10(Counts)") +
  scale_color_brewer(name = "Type", palette = "Dark2", labels = c("N2", "CrA")) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d")
