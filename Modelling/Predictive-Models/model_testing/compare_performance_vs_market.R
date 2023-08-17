#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(fitzRoy)
`%notin%` <- Negate(`%in%`)

# Read in data
neds_fantasy_round_23_2023 <- readRDS("Modelling/Predictive-Models/model_testing/neds_fantasy_round_23_2023.rds")

# Get only diff above 1%
neds_fantasy_round_23_2023 <- neds_fantasy_round_23_2023 |> filter(diff >= 0.01)

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

# Add to df
neds_fantasy_round_23_2023 <-
  neds_fantasy_round_23_2023 |> 
  rowwise() |> 
  mutate(quarter_kelly_wager = kelly_criterion(over_predicted_probability, over_price, 5000, half = FALSE) / 4) |> 
  ungroup()

# Get this weeks stats----------------------------------------------------------
