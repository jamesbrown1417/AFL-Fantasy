#===============================================================================
# Libraries
#===============================================================================

library(brms)
library(rstanarm)
library(tidyverse)
library(mongolite)
library(glmnet)
library(splines)

options(mc.cores = parallel::detectCores())

#===============================================================================
# Functions
#===============================================================================

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

#===============================================================================
# Read in Target
#===============================================================================

target <- read_rds("Modelling/Predictive-Models/Data/target_data_disposals.rds")


#===============================================================================
# Add lines data as predicted margin
#===============================================================================

uri <- Sys.getenv("mongodb_connection_string")

lines_con <- mongo(collection = "Lines", db = "Odds", url = uri)

lines <- lines_con$find('{}') |> tibble()

home_lines_data <-
  lines |>
  separate(match, into = c("home_team", "away_team"), sep = " v ") |>
  select(player_team = home_team, margin = home_line)

away_lines_data <-
  lines |>
  separate(match, into = c("home_team", "away_team"), sep = " v ") |>
  select(player_team = away_team, margin = away_line)

margin_data <-
  bind_rows(home_lines_data, away_lines_data) |>
  mutate(player_team = ifelse(player_team == "West Coast", "West Coast Eagles", player_team)) |>
  mutate(player_team = ifelse(player_team == "Adelaide", "Adelaide Crows", player_team)) |>
  mutate(player_team = ifelse(player_team == "Geelong", "Geelong Cats", player_team)) |>
  mutate(player_team = ifelse(player_team == "Gold Coast", "Gold Coast Suns", player_team)) |>
  mutate(player_team = ifelse(player_team == "Greater Western Sydney", "GWS Giants", player_team)) |>
  mutate(player_team = ifelse(player_team == "Sydney", "Sydney Swans", player_team)) |>
  mutate(margin = -1 * margin)

target <-
  target |>
  left_join(margin_data)

target <- target[complete.cases(target), ]

#===============================================================================
# Create spline terms for margin
#===============================================================================

target$margin_spline <- bs(target$margin, df = 5)

#===============================================================================
# Load Models
#===============================================================================

load("Modelling/Predictive-Models/fitted_models/disposals_stan_poisson.RData")
load("Modelling/Predictive-Models/fitted_models/disposals_stan_neg_binomial2.RData")

##%######################################################%##
#                                                          #
####                  Get Predictions                   ####
#                                                          #
##%######################################################%##

#===============================================================================
# Negative Binomial 2 Model
#===============================================================================

# Generate Samples
pp_samples_3 <- posterior_predict(mod_3, newdata = target, ndraws = 10000)

# Get means
pp_means_3 <- apply(pp_samples_3, 2, mean)

# Bind to target data
target_predictions_3 <- target
target_predictions_3$predicted_disposals = pp_means_3

# Select variables
target_predictions_3 <-
  target_predictions_3 |>
  select(
    player_full_name,
    player_team,
    opposition_team,
    margin,
    round,
    season,
    home_game,
    predicted_disposals
  )

# Create probabilities of 15, 20, 25, 30, 35 and 40 disposals
target_predictions_3$prob_15_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 15))
target_predictions_3$prob_20_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 20))
target_predictions_3$prob_25_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 25))
target_predictions_3$prob_30_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 30))
target_predictions_3$prob_35_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 35))

# Get individual datasets
pred_15_nb <-
  target_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "15+",
            predicted_probability = prob_15_plus)

pred_20_nb <-
  target_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "20+",
            predicted_probability = prob_20_plus)

pred_25_nb <-
  target_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "25+",
            predicted_probability = prob_25_plus)

pred_30_nb <-
  target_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "30+",
            predicted_probability = prob_30_plus)

pred_35_nb <-
  target_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "35+",
            predicted_probability = prob_35_plus)

predicted_probs_nb <-
  bind_rows(pred_15_nb, pred_20_nb, pred_25_nb, pred_30_nb) # Dont bother with 35 for now

# Read in current odds----------------------------------------------------------
disposals_con <- mongo(collection = "Disposals", db = "Odds", url = uri)
disposals <- disposals_con$find('{}') |> tibble()

##%######################################################%##
#                                                          #
#### Compare current odds with predicted probabilities  ####
#                                                          #
##%######################################################%##

# Comparison DF
odds_vs_predicted <-
disposals |> 
  select(player_name, match, number_of_disposals, agency, price,implied_probability,empirical_probability_last_10, empirical_probability_2023) |> 
  left_join(predicted_probs_nb) |> 
  filter(!is.na(predicted_probability)) |> 
  mutate(diff = predicted_probability - implied_probability) |> 
  arrange(desc(diff)) |> 
  group_by(player_name, number_of_disposals) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(desc(diff))
