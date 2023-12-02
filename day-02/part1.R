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
  filter(green <= 13, red <= 12, blue <= 14) |>
  mutate(game_number = as.integer(str_extract(game, "(\\d)+"))) |>
  pull(game_number) |>
  sum() -> answer

str_glue("Part 1: {answer}")
