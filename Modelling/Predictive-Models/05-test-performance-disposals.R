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
target <- read_rds("Modelling/Predictive-Models/Data/target_data_disposals.rds")

#===============================================================================
# Read in historic market prices
#===============================================================================

# Round 21
opening_lines_disposals_round_21 <- read_csv("Modelling/Predictive-Models/model_testing/opening_lines_disposals_round_21.csv")
closing_lines_disposals_round_21 <- read_csv("Modelling/Predictive-Models/model_testing/closing_lines_disposals_round_21.csv")

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
# Poisson Model
#===============================================================================

# Generate Samples
pp_samples_2 <- posterior_predict(mod_2, newdata = test, ndraws = 10000)

# Get means
pp_means_2 <- apply(pp_samples_2, 2, mean)

# Bind to test data
test_predictions_2 <- test
test_predictions_2$predicted_disposals = pp_means_2

# Get squared error
test_predictions_2$squared_error <- (test_predictions_2$disposals - test_predictions_2$predicted_disposals)^2
test_predictions_2$absolute_error <- abs(test_predictions_2$disposals - test_predictions_2$predicted_disposals)

# Select variables
test_predictions_2 <-
  test_predictions_2 |>
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

# Get MSE
MSE_2 <- mean(test_predictions_2$squared_error)
MAE_2 <- mean(test_predictions_2$absolute_error)

# Check posterior predictions vs observed data
pp_check(mod_2)

# Create probabilities of 15, 20, 25, 30, 35 and 40 disposals
test_predictions_2$prob_15_plus <- apply(pp_samples_2, 2, function(x) mean(x >= 15))
test_predictions_2$prob_20_plus <- apply(pp_samples_2, 2, function(x) mean(x >= 20))
test_predictions_2$prob_25_plus <- apply(pp_samples_2, 2, function(x) mean(x >= 25))
test_predictions_2$prob_30_plus <- apply(pp_samples_2, 2, function(x) mean(x >= 30))
test_predictions_2$prob_35_plus <- apply(pp_samples_2, 2, function(x) mean(x >= 35))

# Get individual datasets
pred_15_pois <-
  test_predictions_2 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "15+",
            predicted_probability = prob_15_plus,
            observed_disposals = disposals,
            result = disposals >= 15)

pred_20_pois <-
  test_predictions_2 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "20+",
            predicted_probability = prob_20_plus,
            observed_disposals = disposals,
            result = disposals >= 20)

pred_25_pois <-
  test_predictions_2 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "25+",
            predicted_probability = prob_25_plus,
            observed_disposals = disposals,
            result = disposals >= 25)

pred_30_pois <-
  test_predictions_2 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "30+",
            predicted_probability = prob_30_plus,
            observed_disposals = disposals,
            result = disposals >= 30)

pred_35_pois <-
  test_predictions_2 |>
  transmute(player_name = player_full_name,
            number_of_disposals = "35+",
            predicted_probability = prob_35_plus,
            observed_disposals = disposals,
            result = disposals >= 35)

predicted_probs_pois <-
  bind_rows(pred_15_pois, pred_20_pois, pred_25_pois, pred_30_pois) # Dont bother with 35 for now

# Compare predicted probabilities with those implied by markets-----------------

# Compare with closing lines
poisson_vs_closing_lines <-
  closing_lines_disposals_round_21 |> 
  left_join(predicted_probs_pois) |> 
  mutate(implied_probability = 1/price) |> 
  relocate(implied_probability, .before = predicted_probability) |> 
  filter(!is.na(price) & !is.na(predicted_probability)) |> 
  mutate(diff = predicted_probability - implied_probability) |> 
  relocate(diff, .after = predicted_probability) |>
  arrange(desc(diff))

# Compare with the best lines for each market
poisson_closing_lines_best_markets <-
  poisson_vs_closing_lines |> 
  group_by(player_name, number_of_disposals) |> 
  filter(price == max(price)) |> 
  ungroup()

# Add Kelly Criterion Wager to DF
poisson_closing_lines_best_markets <-
  poisson_closing_lines_best_markets |> 
  rowwise() |> 
  mutate(kelly_wager = kelly_criterion(predicted_probability, price, 10000, half = FALSE) / 4) |> 
  ungroup()

# Calculate profit / loss
poisson_closing_lines_results <-
  poisson_closing_lines_best_markets |> 
  filter(kelly_wager > 0) |> 
  mutate(profit_loss = ifelse(result, kelly_wager*price - kelly_wager, -kelly_wager))

# Get ROI
poisson_closing_lines_roi <- 
  poisson_closing_lines_results |> 
  summarise(total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Poisson", type = "Closing Lines", ROI = round(100*(total_profit_loss / total_staked), 2))

# Get ROI By Market Type
poisson_closing_lines_roi_by_market <- 
  poisson_closing_lines_results |> 
  group_by(number_of_disposals) |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Poisson", type = "Closing Lines", ROI = round(100*(total_profit_loss / total_staked), 2)) |> 
  arrange(desc(ROI))

# Get ROI By Agency
poisson_closing_lines_roi_by_agency <- 
  poisson_closing_lines_results |> 
  group_by(agency) |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Poisson", type = "Closing Lines", ROI = round(100*(total_profit_loss / total_staked), 2)) |> 
  arrange(desc(ROI))

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

# Check posterior predictions vs observed data
pp_check(mod_3)

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
closing_lines_disposals_round_21 |> 
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
  ungroup()

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
  opening_lines_disposals_round_21 |> 
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
  ungroup()

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
  mutate(profit_loss = ifelse(result, kelly_wager*price - kelly_wager, -kelly_wager))

# Get ROI
neg_binomial_opening_lines_roi <- 
  neg_binomial_opening_lines_results |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Negative Binomial", type = "opening Lines", ROI = round(100*(total_profit_loss / total_staked), 2))

# Get ROI By Market Type
neg_binomial_opening_lines_roi_by_market <- 
  neg_binomial_opening_lines_results |> 
  group_by(number_of_disposals) |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Negative Binomial", type = "opening Lines", ROI = round(100*(total_profit_loss / total_staked), 2)) |> 
  arrange(desc(ROI))

# Get ROI By Agency
neg_binomial_opening_lines_roi_by_agency <- 
  neg_binomial_opening_lines_results |> 
  group_by(agency) |> 
  summarise(bets_placed = n(), total_staked = sum(kelly_wager), total_profit_loss = sum(profit_loss)) |> 
  mutate(model = "Negative Binomial", type = "opening Lines", ROI = round(100*(total_profit_loss / total_staked), 2)) |> 
  arrange(desc(ROI))
