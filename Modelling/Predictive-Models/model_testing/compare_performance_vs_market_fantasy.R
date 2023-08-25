#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(fitzRoy)
`%notin%` <- Negate(`%in%`)

# Read in data
neds_fantasy_round_23_2023 <- readRDS("Modelling/Predictive-Models/model_testing/neds_fantasy_round_23_2023.rds")
pb_fantasy_round_23_2023 <- readRDS("Modelling/Predictive-Models/model_testing/pointsbet_fantasy_round_23_2023.rds")

# Get only diff above 1%
neds_fantasy_round_23_2023 <- neds_fantasy_round_23_2023 |> filter(diff >= 0.01)
pb_fantasy_round_23_2023 <- pb_fantasy_round_23_2023 |> filter(diff >= 0.01)

# Calculate Kelly Criterion suggested wager-------------------------------------

kelly_criterion <- function(probability, odds, bankroll, half = TRUE) {
  # Calculate the edge and the odds ratio
  edge <- probability * odds - 1
  odds_ratio <- odds - 1
  
  # If the edge is less than 0, return 0 (i.e., do not bet)
  if (edge <= 0) {
    return(0)
  }
  
  # Calculate the full Kelly fraction
  kelly_fraction <- edge / odds_ratio
  
  # If half is TRUE, bet only half of the Kelly fraction
  if(half) {
    kelly_fraction <- kelly_fraction / 2
  }
  
  # Calculate the bet amount
  bet_amount <- kelly_fraction * bankroll
  
  # Return the bet amount
  return(bet_amount)
}

# Add to dfs
neds_fantasy_round_23_2023 <-
  neds_fantasy_round_23_2023 |> 
  rowwise() |> 
  mutate(quarter_kelly_wager = kelly_criterion(over_predicted_probability, over_price, 5000, half = FALSE) / 4) |> 
  ungroup() |> 
  rename(fantasy_line = fantasy_points)

pb_fantasy_round_23_2023 <-
  pb_fantasy_round_23_2023 |> 
  rowwise() |> 
  mutate(quarter_kelly_wager = kelly_criterion(over_predicted_probability, over_price, 5000, half = FALSE) / 4) |> 
  ungroup() |> 
  rename(fantasy_line = fantasy_points)

# Get this weeks stats----------------------------------------------------------

# Get helper functions
source("Functions/data_processing_functions.r")

# Get data
data_2023 <- get_fantasy_data(season = 2023)

# Select variables and add to prediction df
results_round_23 <-
data_2023 |>
  filter(round == "Round 23") |> 
  select(player_name = player_full_name,
         observed_fantasy_points = fantasy_points)

# Neds--------------------------------------------------------------------------

neds_round_23_summary <-
neds_fantasy_round_23_2023 |> 
  left_join(results_round_23) |> 
  relocate(observed_fantasy_points, .after = fantasy_line) |> 
  mutate(success = observed_fantasy_points >= fantasy_line) |>
  mutate(flat_stake = 100) |> 
  mutate(flat_profit = ifelse(success, flat_stake*over_price - flat_stake, -flat_stake)) |> 
  mutate(kelly_profit = ifelse(success, quarter_kelly_wager*over_price - quarter_kelly_wager, -quarter_kelly_wager))

# Get ROI - flat
neds_round_23_summary |> 
  summarise(total_staked_flat = sum(flat_stake), flat_profit = sum(flat_profit)) |> 
  mutate(ROI_flat = flat_profit / total_staked_flat * 100)

# Get ROI - kelly
neds_round_23_summary |> 
  summarise(total_staked_kelly = sum(quarter_kelly_wager), kelly_profit = sum(kelly_profit)) |> 
  mutate(ROI_kelly = kelly_profit / total_staked_kelly * 100)

# Pointsbet---------------------------------------------------------------------

pb_round_23_summary <-
  pb_fantasy_round_23_2023 |> 
  left_join(results_round_23) |> 
  relocate(observed_fantasy_points, .after = fantasy_line) |> 
  mutate(success = observed_fantasy_points >= fantasy_line) |>
  mutate(flat_stake = 100) |> 
  mutate(flat_profit = ifelse(success, flat_stake*over_price - flat_stake, -flat_stake)) |> 
  mutate(kelly_profit = ifelse(success, quarter_kelly_wager*over_price - quarter_kelly_wager, -quarter_kelly_wager))

# Get ROI - flat
pb_round_23_summary |> 
  summarise(bets_placed = n(), total_staked_flat = sum(flat_stake), flat_profit = sum(flat_profit)) |> 
  mutate(ROI_flat = flat_profit / total_staked_flat * 100)

# Get ROI - kelly
pb_round_23_summary |> 
  summarise(bets_placed = n(), total_staked_kelly = sum(quarter_kelly_wager), kelly_profit = sum(kelly_profit)) |> 
  mutate(ROI_kelly = kelly_profit / total_staked_kelly * 100)
