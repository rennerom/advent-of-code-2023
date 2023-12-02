library(tidyverse)

data <- read_delim(here::here("day-02","input.txt"), delim = ":", col_names = FALSE)
data |> rename("game"  = X1, "results" = X2) -> data

data |>
  separate_longer_delim(results, ";") |>
  separate_longer_delim(results, ",") |>
  mutate(results = str_trim(results)) |>
  separate_wider_delim(results, " ", names = c("count", "color")) |>
  mutate(count = as.numeric(count)) |>
  pivot_wider(names_from = color, values_from = count, values_fn = max) |>
  mutate(power = green * red * blue) |>
  pull(power) |>
  sum() -> answer

str_glue("Part 2: {answer}")

