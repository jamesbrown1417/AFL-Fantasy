# Get functions to determine player correlations based on historical data

#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(tidytable)
library(stringr)
library(fitzRoy)
`%notin%` <- Negate(`%in%`)

source("Functions/data_processing_functions.R")

str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

#===============================================================================
# Get historical data
#===============================================================================

# 2014 - 2022
afl_fantasy_2014_2022_data <- readRDS("C:/Users/a1645197/AFL-Fantasy/Data/afl_fantasy_2014_2022_data.rds")

# 2023
afl_fantasy_2023_data <- get_fantasy_data(season = 2023)

# Combine
player_stats <- bind_rows(afl_fantasy_2014_2022_data, afl_fantasy_2023_data)

#===============================================================================
# Create functions to get correlation tables
#===============================================================================

get_player_correlations <- function(player_a, player_b, line_a, line_b, seasons = 2014:2023) {
  # Individual DFs
  a_data <- player_stats |>
    filter(player_full_name == player_a) |> 
    select(season_name, round, player_team, player_a_disposals = disposals) |> 
    mutate(season_name = as.numeric(season_name))
  
  b_data <- player_stats |>
    filter(player_full_name == player_b) |> 
    select(season_name, round, player_team, player_b_disposals = disposals) |> 
    mutate(season_name = as.numeric(season_name))
  
  # Get games where players played together
  combined_data <- inner_join(a_data, b_data) |> filter(season_name %in% seasons)
  
  # Get games where player A covered line
  player_a_covered <- combined_data |> filter(player_a_disposals >= line_a)
  
  # Get games where player B covered line
  player_b_covered <- combined_data |> filter(player_b_disposals >= line_b)
  
  # Get unconditional probabilities
  player_a_prob <- mean(combined_data$player_a_disposals >= line_a)
  player_b_prob <- mean(combined_data$player_b_disposals >= line_b)
  
  # Get conditional probabilities
  player_a_cond_prob <- mean(player_b_covered$player_a_disposals >= line_a)
  player_b_cond_prob <- mean(player_a_covered$player_b_disposals >= line_b)
  
  # Create table
  table <-
  tibble(
    player = c(player_a, player_b),
    n = nrow(combined_data),
    disposal_line = c(line_a, line_b),
    probability = c(player_a_prob, player_b_prob),
    conditional_probability = c(player_a_cond_prob, player_b_cond_prob)
  )
  
  # Perform a fisher exact test
  new_dat <-
    combined_data |> 
    mutate(player_a_success = player_a_disposals >= line_a,
           player_b_success = player_b_disposals >= line_b)
  
  test <- fisher.test(new_dat$player_a_success, new_dat$player_b_success)
  
  # Calculate estimated multi price
  est_price_1 = 1 / (table[[1, 4]] * table[[2, 5]])
  est_price_2 = 1 / (table[[2, 4]] * table[[1, 5]])
  
  # Add to table
  table$est_price = 1 / table$probability
  table$est_multi_price = c(est_price_1, est_price_2)
  table$p_value = test$p.value
  
  # Output
  table
}

#===============================================================================
# Apply function to player combinations
#===============================================================================

# Player combinations for this year
player_combinations <-
afl_fantasy_2023_data |>
  distinct(player_full_name, player_team) |>
  cross_join(afl_fantasy_2023_data |>
               distinct(player_full_name, player_team)) |> 
  filter(player_team.x == player_team.y) |> 
  filter(player_full_name.x != player_full_name.y) |> 
  mutate(id = paste0(player_full_name.x, player_full_name.y)) |> 
  mutate(id = str_remove_all(id, " ")) |>   
  mutate(id = str_to_lower(id)) |> 
  mutate(id = str_arrange(id)) |> 
  distinct(id, .keep_all = TRUE) |> 
  select(player_a = player_full_name.x, player_b = player_full_name.y)

# Get version of function that will handle errors
corr_fun <- possibly(get_player_correlations, "error")

# Apply function - 15 disposals-------------------------------------------------
correlation_list_15 <-
  map2(
    .x = player_combinations$player_a,
    .y = player_combinations$player_b,
    .f = corr_fun,
    line_a = 15,
    line_b = 15,
    seasons = 2021:2023
  )

# Remove non df elements
correlation_list_15 <-
  correlation_list_15 |> 
  keep(is.data.frame)

# extract the minimum value of the "p_value" column from each data frame
min_values <- map_dbl(correlation_list_15, ~ min(.x$p_value))

# order the list based on the extracted maximum values
correlation_list_15 <- correlation_list_15[order(min_values, decreasing = FALSE)]

# Apply function - 15 v 20 disposals--------------------------------------------
correlation_list_15_20 <-
  map2(
    .x = player_combinations$player_a,
    .y = player_combinations$player_b,
    .f = corr_fun,
    line_a = 15,
    line_b = 20,
    seasons = 2021:2023
  )

# Remove non df elements
correlation_list_15_20 <-
  correlation_list_15_20 |> 
  keep(is.data.frame)

# extract the minimum value of the "p_value" column from each data frame
min_values <- map_dbl(correlation_list_15_20, ~ min(.x$p_value))

# order the list based on the extracted maximum values
correlation_list_15_20 <- correlation_list_15_20[order(min_values, decreasing = FALSE)]

# Apply function - 20 v 15 disposals--------------------------------------------
correlation_list_20_15 <-
  map2(
    .x = player_combinations$player_a,
    .y = player_combinations$player_b,
    .f = corr_fun,
    line_a = 20,
    line_b = 15,
    seasons = 2021:2023
  )

# Remove non df elements
correlation_list_20_15 <-
  correlation_list_20_15 |> 
  keep(is.data.frame)

# extract the minimum value of the "p_value" column from each data frame
min_values <- map_dbl(correlation_list_20_15, ~ min(.x$p_value))

# order the list based on the extracted maximum values
correlation_list_20_15 <- correlation_list_20_15[order(min_values, decreasing = FALSE)]

# Apply function - 20 disposals-------------------------------------------------
correlation_list_20 <-
  map2(
    .x = player_combinations$player_a,
    .y = player_combinations$player_b,
    .f = corr_fun,
    line_a = 20,
    line_b = 20,
    seasons = 2021:2023
  )

# Remove non df elements
correlation_list_20 <-
  correlation_list_20 |> 
  keep(is.data.frame)

# extract the minimum value of the "p_value" column from each data frame
min_values <- map_dbl(correlation_list_20, ~ min(.x$p_value))

# order the list based on the extracted maximum values
correlation_list_20 <- correlation_list_20[order(min_values, decreasing = FALSE)]

# Apply function - 25 disposals-------------------------------------------------
correlation_list_25 <-
  map2(
    .x = player_combinations$player_a,
    .y = player_combinations$player_b,
    .f = corr_fun,
    line_a = 25,
    line_b = 25,
    seasons = 2021:2023
  )

# Remove non df elements
correlation_list_25 <-
  correlation_list_25 |> 
  keep(is.data.frame)

# extract the minimum value of the "p_value" column from each data frame
min_values <- map_dbl(correlation_list_25, ~ min(.x$p_value))

# order the list based on the extracted maximum values
correlation_list_25 <- correlation_list_25[order(min_values, decreasing = FALSE)]
