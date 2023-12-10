library(tidyverse)

# set up
data <-
  read_lines('day-05/input.txt') |>
  str_replace_all("(^)\\d", ":\\0") |> tibble() |>
  separate(col = 1, into = c("map", "values"), sep = ":") |>
  mutate_all(~ifelse(.x == "", NA, .x)) |>
  mutate(values = str_trim(values)) |>
  fill(map) |> drop_na() |> group_by(map) |> nest() |> ungroup() |>
  mutate(map = str_replace_all(map, " map", ""))

seeds <- data |> filter(map == 'seeds') |> unnest(data) |>
  separate_wider_delim(values, delim = " ", names_sep = "_") |>
  pivot_longer(-1, values_to = "value") |>
  select(-name, -map, seed_num = value) |>
  mutate(seed_num = as.numeric(seed_num))

mappings <-
  data |> filter(map != 'seeds') |>
  mutate(
    data = map(
      data, ~separate_wider_delim(
        .x,
        values,
        delim = " ",
        names = c("dest_start", "src_start", "rng_len")
      ) |>
        mutate_at(c("dest_start", "src_start", "rng_len"), as.numeric) |>
        mutate(
          dest_end = dest_start + rng_len - 1,
          src_end = src_start + rng_len - 1,
          offset = src_start - dest_start
        ) |>
        select(dest_start, dest_end, src_start, src_end, offset) |>
        arrange(dest_start)
    ),
    row_num = row_number()
  )

# functions
get_seed_value <- function(val, map) {
  res <- map |>
    map(~filter(.x, src_start <= val & src_end >= val)) |>
    simplify() |> first() |> pull(offset)

  if (is_empty(res)) {
    new_val <- val
  } else {
    new_val <- val - res
  }
  return(new_val)
}

get_new_seed_values_list <- function(seed_list, map) {
  temp_list <- list()
  for (i in seed_list) {
    val <- get_seed_value(i, map)
    temp_list <- append(temp_list, val)
  }
  return(temp_list)
}

calculate_min_value <- function(original_seeds) {
  current_list <- original_seeds
  for (i in 1:7) {
    current_list <- get_new_seed_values_list(current_list, mappings$data[i])
  }
  min <- current_list |> unlist() |> min()
  return(min)
}

# results
str_glue("Min value: {calculate_min_value(as.list(seeds$seed_num))}")

