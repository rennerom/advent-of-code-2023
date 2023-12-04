library(tidyverse)

data <- read_delim(here::here("day-03","input.txt"), delim = " ", col_names = FALSE) |>
  rename('code' = X1)

# build a list of numbers in a string
find_nums <- function(x) {
  nums <- str_replace_all(x, "[^0-9]", " ") |>
    str_trim() |>
    str_split(" +") |>
    unlist() |>
    map_int(~as.numeric(.x))
  return(nums)
}

# get indexes of symbols in a string
find_flags_index <- function(x) {
  str_locate_all(x, "[^0-9.]")[[1]][,1]
}

make_row_tibble <- function(main_row, compare_row_one, compare_row_two) {

  temp_tibble <- tibble()

  # list of symbol indexes to compare for each row relative to the main row
  indexes_one <- find_flags_index(compare_row_one)
  indexes_two <- find_flags_index(compare_row_two)
  indexes_main_row <- find_flags_index(main_row) # include main row for comparison as well (ei: ...$435.. needs to be included)

  nums <- find_nums(main_row)

  # loop through each number in the main row
  # and get indexes of individual number characters
  # Example, "...123...334" would convert to
  #    index | value
  # ----------------
  #  4, 5, 6 |   123
  # 10,11,12 |   334

  for (i in nums) {
    num <- as.character(i)
    num <- str_glue("(?<!\\d){num}(?!\\d)") # fixes the issue of matching 123 when looking for 12
    res <- str_locate_all(main_row, num)[[1]] |>
      as_tibble() |>
      mutate(val = i)
    temp_tibble <- bind_rows(temp_tibble, res)
  }

  # add extra indexes to pad the left and right of the number by one position
  # since the symbols don't need to overlap, they can be adjacent to the number
  # Example, "...123...334" would convert to
  #          index | value
  # ----------------------
  # 3, 4, 5, 6,  7 |   123
  # 9,10,11,12, 13 |   334
  temp_tibble <- temp_tibble |>
    distinct() |>
    arrange(start) |>
    mutate(
      range_list = map2(pmax(start-1,1), pmin(end+1,140), ~seq(.x, .y, 1)),
      flags_list_one = list(indexes_one),
      flags_list_two = list(indexes_two),
      flags_list_main = list(indexes_main_row),

      # logical flags for if the number is in the range of the symbols
      include_one = map2_lgl(range_list, flags_list_one, ~sum(.x %in% .y)),
      include_two = map2_lgl(range_list, flags_list_two, ~sum(.x %in% .y)),
      include_main = map2_lgl(range_list, flags_list_main, ~sum(.x %in% .y)),

      # return the number value if it is in the range of the symbols or 0 if not
      num = ifelse(include_one | include_two | include_main, val, 0)
    ) |>
    # return a string of space separated numbers that ARE in the range of the symbol indexes
    filter(num != 0) |> # but filter out zeros
    pull(num) |>
    str_c(collapse = " ")
  return(temp_tibble)
}

# loop through each row and compare it to the previous row, next row, and itself
# to get the numbers that are in the range of the symbol indexes.
# if it's the first row, just compare it to the next row twice. That won't matter.
# if it's the last row, just compare it to the previous row twice. That won't matter either.
results_tibble <- tibble()
for (i in 1:140){
  i_plus_one <- pmin(i + 1,140)
  i_minus_one <- pmax(i - 1,1)
  results <- make_row_tibble(data$code[i], data$code[i_minus_one], data$code[i_plus_one]) |> as_tibble()
  results <- results |> mutate(code = data$code[i]) |> select(code, everything())
  results_tibble <- bind_rows(results_tibble, results)
}

# main results with extra columns to help with debugging
main_tibble <- results_tibble |>
  select(value) |>
  mutate(
    values_sum = map_int(value, ~str_replace_all(.x, "[^0-9]", " ") |>
                           str_trim() |>
                           str_split(" +") |>
                           unlist() |>
                           map_int(~as.numeric(.x)) |>
                           sum())
  )

main_tibble |> pull(values_sum) |> sum()
