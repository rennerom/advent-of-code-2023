data <- read_delim(here::here("day-01","input.txt"), delim = " ", col_names = FALSE)
data |> rename('code' = X1) -> data
num_str = list("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
num_val = list("1", "2", "3", "4", "5", "6", "7", "8", "9")


find_matches_num_str <- function(code, list_of_nums, list_of_vals) {
  all_matches <- tibble()
  for (i in 1:length(list_of_nums)) {
    pos <- str_locate_all(code, list_of_nums[[i]])
    pos <- pos[[1]][,1]
    val <- list_of_vals[[i]]
    num <- list_of_nums[[i]]
    tibble(pos, val) -> res_tibble
    if (length(pos) > 0) {
      all_matches <- bind_rows(all_matches, res_tibble)
    }
  }
  return(all_matches)
}

build_matches <- function(code, num_strs, num_vals) {
  strs_matches <- find_matches_num_str(code, num_strs, num_vals)
  vals_matches <- find_matches_num_str(code, num_vals, num_vals)
  matches <- bind_rows(strs_matches, vals_matches)
  matches <- matches |>
    arrange(pos)
  first_match <- matches[1,]$val
  last_match <- matches[nrow(matches),]$val
  value <- as.integer(glue::glue("{first_match}{last_match}"))
  return(tibble(code, first_match, last_match, value))
  # return(matches)
}

total_tibble <- tibble()
for (c in data$code){
  results <- build_matches(c, num_str, num_val)
  total_tibble <- bind_rows(total_tibble, results)
}
total_tibble

total_tibble |> pull(value) |> sum()
