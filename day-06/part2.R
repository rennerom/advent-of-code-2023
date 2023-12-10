library(tidyverse)

data <- read_delim("day-06/input.txt", delim = ":", col_names = FALSE)
data |>
  rename(name = X1, value = X2) |>
  # remove all spaces from value column
  mutate(value = as.numeric(str_replace_all(value, " ", ""))) |>
  pivot_wider(names_from = name, values_from = value, values_fn = list) |>
  unnest(c(Time, Distance)) |>
  mutate(
    times = map(Time, ~(seq(1, .x, 1))),
    distances = map2(times, Time, ~(.x * (.y - .x))),
    win = map2(distances, Distance, ~.x > .y),
    win_count = map_int(win, ~sum(.x))
  ) |>
  pull(win_count) |>
  reduce(`*`)



