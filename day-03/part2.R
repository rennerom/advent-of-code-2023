library(tidyverse)

data <- read_delim(here::here("day-03","input.txt"), delim = " ", col_names = FALSE) |>
  rename('code' = X1)

data

find_gears <- function(x) {
  str_locate_all(x, "\\*")[[1]][,1]
}

find_nums <- function(x) {
  nums <- str_replace_all(x, "[^0-9]", " ") |>
    str_trim() |>
    str_split(" +") |>
    unlist() |>
    map_int(~as.numeric(.x))
  return(nums)
}
temp_tibble <- tibble()
for (i in nums) {
  num <- as.character(i)
  num <- str_glue("(?<!\\d){num}(?!\\d)") # fixes the issue of matching 123 when looking for 12
  res <- str_locate_all(data$code[4], num)[[1]] |>
    as_tibble() |>
    mutate(val = i)
  temp_tibble <- bind_rows(temp_tibble, res)
}
temp_tibble
find_gears(data$code[4])
find_nums(data$code[4])
