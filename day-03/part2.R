library(tidyverse)

data <- read_delim(here::here("day-03","input.txt"), delim = " ", col_names = FALSE) |>
  rename('code' = X1)

data

find_inline_gears_total <- function(x) {
  str_extract_all(x, "\\d+\\*\\d+")[[1]] |>
    map(~str_split(.x, "\\*")[[1]] |> map_int(~as.numeric(.x)) |> prod()) |>
    map_int(~.x) |> sum() -> result
  return(result)
}

# compare two lines
one <- data$code[1]
two <- data$code[2]

one |> str_replace("[^\\D*]", ".")
two |> str_replace("[^\\D*]", ".")

