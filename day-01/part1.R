library(tidyverse)

data <- read_delim(here::here("day-01","input.txt"), delim = " ", col_names = FALSE)

# rename columns
data |> rename('code' = X1) -> data

parse_string <- function(x) {
  x |>
    str_to_lower() |> # force lower case just in case
    str_remove_all("\\D+") |>
    str_split("") -> x # convert to a list of chars
  first_element <- x[[1]][1]
  last_element <- x[[1]][length(x[[1]])]
  val <- as.integer(glue::glue("{first_element}{last_element}"))
  return(val)
}

data |> mutate(
  value = map_int(code, parse_string)
) |>
  pull(value) |>
  sum()
