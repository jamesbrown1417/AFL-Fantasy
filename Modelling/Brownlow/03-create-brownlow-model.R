##%######################################################%##
#                                                          #
####       Create Brownlow Medal prediction model       ####
#                                                          #
##%######################################################%##

# libraries
library(MASS)
library(tidyverse)

# Read in data
brownlow_analysis_data <-
  read_rds("Modelling/Brownlow/brownlow_analysis_data.rds") |>
  mutate(brownlow_votes = ifelse(is.na(brownlow_votes), 0, brownlow_votes),
         coaches_votes = ifelse(is.na(coaches_votes), 0, coaches_votes)) |>
  mutate(brownlow_votes = factor(brownlow_votes, levels = c(0,1,2,3), ordered = TRUE))

# Split into test and training data
test <- brownlow_analysis_data |> filter(Season == 2023) |> select(-brownlow_votes)
training <- brownlow_analysis_data |> filter(Season != 2023)

##%######################################################%##
#                                                          #
####             Fit model on training data             ####
#                                                          #
##%######################################################%##

# Fit Model
pred_model <-
  polr(
    brownlow_votes ~
      coaches_votes +
      disposal_efficiency +
      contested_possessions +
      uncontested_possessions +
      kicks +
      handballs +
      marks +
      tackles +
      rating_points +
      metres_gained +
      goals +
      goal_assists +
      score_involvements +
      score_launches + 
      centre_clearances +
      stoppage_clearances +
      intercept_marks +
      intercepts +
      one_percenters +
      pressure_acts +
      inside_fifties +
      rebounds +
      margin +
      supercoach,
    data = training,
    Hess = TRUE
  )

# Generate predicted probabilities
predicted_probabilities <- predict(pred_model, newdata = test, type = "probs")

# Join with test data labels
test_predictions <-
  test |>
  select(player_name, start_time, match, Round, coaches_votes) |>
  bind_cols(predicted_probabilities) |>
  mutate(expected_votes = 1*`1` + 2*`2` + 3*`3`) |>
  group_by(match, Round) |>
  filter(!is.na(expected_votes)) |> 
  mutate(expected_votes_normalised = expected_votes / sum(expected_votes)) |>
  arrange(start_time, match, desc(expected_votes)) |>
  ungroup()

# Get most likely vote count
most_likely_vote_count <-
test_predictions |>
  arrange(start_time, match, Round, expected_votes) |> 
  group_by(match, Round) |> 
  slice_tail( n = 3) |> 
  mutate(predicted_votes = row_number()) |> 
  group_by(player_name) |> 
  summarise(total_votes = sum(predicted_votes)) |> 
  arrange(desc(total_votes))

# Function to get 3, 2 and 1 votes from each match
get_votes <- function(match_name, round) {
  # Get selected match
  match_data <-
  test_predictions |>
    filter(match == match_name, round == Round)
  
  # Perform sample votes count for match
  pred_votes <- sample(match_data$player_name, size = 3, replace = FALSE, prob = match_data$expected_votes)
  
  # Create tibble
  tibble(player_name = pred_votes, match = match_name, round = round, brownlow_votes = 3:1)
}

# Get distinct match numbers and rounds
matches_2023 <-
  distinct(test_predictions, match, Round) |>
  rename(match_name = match, round = Round)

# Map over each match and round to get brownlow votes dataset and repeat 1000 times
empty_list = list()

# Populate list
for (i in 1:1000) {
  to_add <-
  pmap(matches_2023, get_votes) |>
  bind_rows() |>
  group_by(player_name) |>
  summarise(vote_tally = sum(brownlow_votes)) |>
  arrange(desc(vote_tally))
  
  # Add 'to_add' to 'empty_list'
  empty_list[[i]] <- to_add
}

# Bind rows of empty list and get averages
avg_preds <-
  bind_rows(empty_list) |>
  group_by(player_name) |>
  summarise(mean = mean(vote_tally),
            median = median(vote_tally),
            `25th percentile` = quantile(vote_tally, 0.25),
            `75th percentile` = quantile(vote_tally, 0.75)) |>
  arrange(desc(mean))

result <-
  map(empty_list, ~ .x |>
        arrange(desc(vote_tally)) |>
        slice_head(n = 1)) |>
  bind_rows() |>
  group_by(player_name) |>
  tally() |> 
  arrange(desc(n)) |> 
  mutate(n = n / sum(n))

# Get by team probability-------------------------------------------------------

# # Get player team data
# player_teams <-
# test |>
#   separate(match, into = c("home_team", "away_team", sep = " v ", remove = TRUE)) |>
#   group_by(player_name, home_team) |>
#   tally() |>
#   arrange(desc(n)) |>
#   slice_head(n = 1) |>
#   select(player_name, player_team = home_team)

# # Get most votes by team
# bind_rows(empty_list) |>
#   left_join(player_teams) |>
#   filter(player_team == "North") |>
#   group_by(player_name) |>
#   summarise(avg_votes = mean(vote_tally)) |>
#   arrange(desc(avg_votes))
