library(tidyverse)

data <- read_delim(here::here("day-04","input.txt"), delim = ":", col_names = FALSE)

data |>
  separate_wider_delim(col = X2, delim = "|", names = c("winning_nums","card_nums")) |>
  rename("game" = X1) |>
  mutate(
    winning_nums = str_squish(winning_nums),
    winning_list = strsplit(winning_nums, " ") |> map(~as.integer(.x)),
    card_nums = str_squish(card_nums),
    card_list = strsplit(card_nums, " ") |> map(~as.integer(.x)),
    matches = map2(winning_list, card_list, ~intersect(.x, .y)) |> map_int(~length(.x)),
    score = ifelse(matches == 0, 0, 2**(matches-1))
  ) -> data2

data2 |> select(matches) |>
  mutate(include = ifelse(matches > 0, 1, 0)) -> data3

data3$matches[1]
res <- tibble()
for (i in 1:204) {
  start <- i+1
  end <- data3$matches[i] + i
  sum <- data3[start:end,]
  # append to res
  res <- bind_rows(res, sum)
  res |> mutate(card = i) -> res
}

res |> str()
