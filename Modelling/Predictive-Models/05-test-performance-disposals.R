##%######################################################%##
#                                                          #
####           Load in fitted Stan models and           ####
#### make probabilistic predictions for disposals data  ####
#                                                          #
##%######################################################%##

# Test how well the models do vs historic data

#===============================================================================
# Libraries
#===============================================================================

library(brms)
library(rstanarm)
library(tidyverse)
library(mongolite)
library(glmnet)
library(splines)

`%notin%` <- Negate(`%in%`)

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
# Read in Training, Target and Test Data
#===============================================================================

training <- read_rds("Modelling/Predictive-Models/Data/training_data_disposals.rds")
test <- read_rds("Modelling/Predictive-Models/Data/test_data_disposals.rds")

#===============================================================================
# Create spline terms for margin
#===============================================================================

training$margin_spline <- bs(training$margin, df = 5)
test$margin_spline <- bs(test$margin, df = 5)

#===============================================================================
# Read in historic market prices
#===============================================================================

# Round 22
opening_lines_disposals_round_22 <- read_csv("Modelling/Predictive-Models/model_testing/opening_lines_disposals_round_22.csv")
closing_lines_disposals_round_22 <- read_csv("Modelling/Predictive-Models/model_testing/closing_lines_disposals_round_22.csv")

# Ignore certain players (i.e. role change or just come back from injury)
players_to_ignore <- c("Clayton Oliver", "Jamaine Jones", "Ben Keays", "Jaeger O'Meara")

opening_lines_disposals_round_22 <-
  opening_lines_disposals_round_22 |> 
  filter(player_name %notin% players_to_ignore)

closing_lines_disposals_round_22 <-
  closing_lines_disposals_round_22 |> 
  filter(player_name %notin% players_to_ignore)

#===============================================================================
# Load Models
#===============================================================================

load("Modelling/Predictive-Models/fitted_models/disposals_stan_poisson.RData")
load("Modelling/Predictive-Models/fitted_models/disposals_stan_neg_binomial2.RData")

#===============================================================================
# Compare models using LOO
#===============================================================================

# loo_pois <- loo(mod_2, cores = 2)
# loo_nb <- loo(mod_3, cores = 2)
# 
# loo_compare(loo_pois, loo_nb)

# Above we see that the negative binomial model is the preferred model.

##%######################################################%##
#                                                          #
####                  Get Predictions                   ####
#                                                          #
##%######################################################%##

#===============================================================================
# Negative Binomial 2 Model
#===============================================================================

# Generate Samples
pp_samples_3 <- posterior_predict(mod_3, newdata = test, ndraws = 10000)

# Get means
pp_means_3 <- apply(pp_samples_3, 2, mean)

# Bind to test data
test_predictions_3 <- test
test_predictions_3$predicted_disposals = pp_means_3

# Get squared error
test_predictions_3$squared_error <- (test_predictions_3$disposals - test_predictions_3$predicted_disposals)^2
test_predictions_3$absolute_error <- abs(test_predictions_3$disposals - test_predictions_3$predicted_disposals)

# Select variables
test_predictions_3 <-
  test_predictions_3 |>
  select(
    player_full_name,
    player_team,
    opposition_team,
    margin,
    round,
    season,
    home_game,
    disposals,
    predicted_disposals,
    squared_error,
    absolute_error
  )

# Create probabilities of 15, 20, 25, 30, 35 and 40 disposals
test_predictions_3$prob_15_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 15))
test_predictions_3$prob_20_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 20))
test_predictions_3$prob_25_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 25))
test_predictions_3$prob_30_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 30))
test_predictions_3$prob_35_plus <- apply(pp_samples_3, 2, function(x) mean(x >= 35))

# Get individual datasets
pred_15_nb <-
  test_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "15+",
            predicted_probability = prob_15_plus,
            observed_disposals = disposals,
            result = disposals >= 15)

pred_20_nb <-
  test_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "20+",
            predicted_probability = prob_20_plus,
            observed_disposals = disposals,
            result = disposals >= 20)

pred_25_nb <-
  test_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "25+",
            predicted_probability = prob_25_plus,
            observed_disposals = disposals,
            result = disposals >= 25)

pred_30_nb <-
  test_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "30+",
            predicted_probability = prob_30_plus,
            observed_disposals = disposals,
            result = disposals >= 30)

pred_35_nb <-
  test_predictions_3 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "35+",
            predicted_probability = prob_35_plus,
            observed_disposals = disposals,
            result = disposals >= 35)

predicted_probs_nb <-
  bind_rows(pred_15_nb, pred_20_nb, pred_25_nb, pred_30_nb) # Dont bother with 35 for now

# Compare predicted probabilities with those implied by markets-----------------

#============================#
# Compare with closing lines #
#============================#

negative_binomial_vs_closing_lines <-
closing_lines_disposals_round_22 |> 
  left_join(predicted_probs_nb) |> 
  mutate(implied_probability = 1/price) |> 
  relocate(implied_probability, .before = predicted_probability) |> 
  filter(!is.na(price) & !is.na(predicted_probability)) |> 
  mutate(diff = predicted_probability - implied_probability) |> 
  relocate(diff, .after = predicted_probability) |>
  arrange(desc(diff))

# Compare with the best lines for each market
neg_binomial_closing_lines_best_markets <-
  negative_binomial_vs_closing_lines |> 
  group_by(player_name, number_of_disposals) |> 
  filter(price == max(price)) |> 
  ungroup() |> 
  distinct(player_name, number_of_disposals, .keep_all = TRUE)

# Add Kelly Criterion Wager to DF
neg_binomial_closing_lines_best_markets <-
  neg_binomial_closing_lines_best_markets |> 
  rowwise() |> 
  mutate(kelly_wager = kelly_criterion(predicted_probability, price, 10000, half = FALSE) / 4) |> 
  ungroup()

# Calculate profit / loss
neg_binomial_closing_lines_results <-
  neg_binomial_closing_lines_best_markets |> 
  filter(kelly_wager > 0) |> 
  filter(price >= 1.10) |> 
  filter(diff >= 0.02) |>
  mutate(profit_loss = ifelse(result, kelly_wager*price - kelly_wager, -kelly_wager))

# Get ROI
neg_binomial_closing_lines_roi <- 
  neg_binomial_closing_lines_results |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Negative Binomial", type = "Closing Lines", ROI = round(100*(total_profit_loss / total_staked), 2))

# Get ROI By Market Type
neg_binomial_closing_lines_roi_by_market <- 
  neg_binomial_closing_lines_results |> 
  group_by(number_of_disposals) |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Negative Binomial", type = "Closing Lines", ROI = round(100*(total_profit_loss / total_staked), 2)) |> 
  arrange(desc(ROI))

# Get ROI By Agency
neg_binomial_closing_lines_roi_by_agency <- 
  neg_binomial_closing_lines_results |> 
  group_by(agency) |>
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Negative Binomial", type = "Closing Lines", ROI = round(100*(total_profit_loss / total_staked), 2)) |> 
  arrange(desc(ROI))

#============================#
# Compare with opening lines #
#============================#

negative_binomial_vs_opening_lines <-
  opening_lines_disposals_round_22 |> 
  left_join(predicted_probs_nb) |> 
  mutate(implied_probability = 1/price) |> 
  relocate(implied_probability, .before = predicted_probability) |> 
  filter(!is.na(price) & !is.na(predicted_probability)) |> 
  mutate(diff = predicted_probability - implied_probability) |> 
  relocate(diff, .after = predicted_probability) |>
  arrange(desc(diff))

# Compare with the best lines for each market
neg_binomial_opening_lines_best_markets <-
  negative_binomial_vs_opening_lines |> 
  group_by(player_name, number_of_disposals) |>
  filter(price == max(price)) |>
  ungroup() |>
  distinct(player_name, number_of_disposals, .keep_all = TRUE)

# Add Kelly Criterion Wager to DF
neg_binomial_opening_lines_best_markets <-
  neg_binomial_opening_lines_best_markets |> 
  rowwise() |> 
  mutate(kelly_wager = kelly_criterion(predicted_probability, price, 10000, half = FALSE) / 4) |> 
  ungroup()

# Calculate profit / loss
neg_binomial_opening_lines_results <-
  neg_binomial_opening_lines_best_markets |> 
  filter(kelly_wager > 0) |> 
  filter(price >= 1.10) |> 
  filter(diff >= 0.02) |>
  mutate(profit_loss = ifelse(result, kelly_wager*price - kelly_wager, -kelly_wager))

# Get ROI
neg_binomial_opening_lines_roi <- 
  neg_binomial_opening_lines_results |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Negative Binomial", type = "Opening Lines", ROI = round(100*(total_profit_loss / total_staked), 2))

# Get ROI By Market Type
neg_binomial_opening_lines_roi_by_market <- 
  neg_binomial_opening_lines_results |> 
  group_by(number_of_disposals) |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Negative Binomial", type = "Opening Lines", ROI = round(100*(total_profit_loss / total_staked), 2)) |> 
  arrange(desc(ROI))

# Get ROI By Agency
neg_binomial_opening_lines_roi_by_agency <- 
  neg_binomial_opening_lines_results |> 
  group_by(agency) |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Negative Binomial", type = "Opening Lines", ROI = round(100*(total_profit_loss / total_staked), 2)) |> 
  arrange(desc(ROI))
