# Create the directory structure

library(purrr)

make_dir <- function(dirname) {
  if (!dir.exists(dirname)) {
    dir.create(dirname)
  }
}

data_dir <- "data"
results_dir <- "results"
figures_dir <- "figures"

all_dirs <- c(data_dir, results_dir, figures_dir)

walk(all_dirs, make_dir)
