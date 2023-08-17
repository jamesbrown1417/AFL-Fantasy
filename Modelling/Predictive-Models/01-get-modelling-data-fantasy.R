#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(fitzRoy)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in required data
#===============================================================================


# Get helper functions
source("Functions/data_processing_functions.r")

# Get data
data_2023 <- get_fantasy_data(season = 2023)
afl_fantasy_2014_2022_data <- readRDS("Data/afl_fantasy_2014_2022_data.rds")

# Combine
all_data <-
  bind_rows(data_2023, afl_fantasy_2014_2022_data) |> 
  arrange(start_time_utc)

# Make round number an ordered factor
all_data$round <-
  factor(
    all_data$round,
    levels = c(
      'Round 1',
      'Round 2',
      'Round 3',
      'Round 4',
      'Round 5',
      'Round 6',
      'Round 7',
      'Round 8',
      'Round 9',
      'Round 10',
      'Round 11',
      'Round 12',
      'Round 13',
      'Round 14',
      'Round 15',
      'Round 16',
      'Round 17',
      'Round 18',
      'Round 19',
      'Round 20',
      'Round 21',
      'Round 22',
      'Round 23',
      'Round 24',
      'Finals Week 1',
      'Semi Finals',
      'Preliminary Finals',
      'Grand Final'
    ),
    ordered = TRUE
  )

# Get positions data
positions <-
  read_rds("Data/afl_clustering_positions.rds") |>
  mutate(round = paste("Round", round)) |>
  select(player_name, season, round, position, position)

# Make round number an ordered factor
positions$round <-
  factor(
    positions$round,
    levels = c(
      'Round 1',
      'Round 2',
      'Round 3',
      'Round 4',
      'Round 5',
      'Round 6',
      'Round 7',
      'Round 8',
      'Round 9',
      'Round 10',
      'Round 11',
      'Round 12',
      'Round 13',
      'Round 14',
      'Round 15',
      'Round 16',
      'Round 17',
      'Round 18',
      'Round 19',
      'Round 20',
      'Round 21',
      'Round 22',
      'Round 23',
      'Round 24',
      'Finals Week 1',
      'Semi Finals',
      'Preliminary Finals',
      'Grand Final'
    ),
    ordered = TRUE
  )

# Use all data from 2021 onwards
fantasy_all <-
all_data |>
  filter(season_name %in% c("2021", "2022", "2023")) |> 
  filter(tog_percentage > 50) |> 
  mutate(season_name = as.numeric(season_name))

# Make Margin Variable negative if a loss
fantasy_all <-
  fantasy_all |> 
  mutate(margin = if_else(player_team == away_team & match_result == "Home Win", -margin, margin)) |> 
  mutate(margin = if_else(player_team == home_team & match_result == "Away Win", -margin, margin)) 

# Create lag variables
# Create lag variables
fantasy_all <- fantasy_all |>
  arrange(player_full_name, player_team, start_time_utc) |>
  group_by(player_full_name, player_team) |>
  mutate(
    fantasy_lag_1 = lag(fantasy_points, 1),
    fantasy_lag_2 = lag(fantasy_points, 2),
    fantasy_lag_3 = lag(fantasy_points, 3),
    fantasy_lag_4 = lag(fantasy_points, 4),
    fantasy_lag_5 = lag(fantasy_points, 5),
    fantasy_lag_6 = lag(fantasy_points, 6),
    fantasy_lag_7 = lag(fantasy_points, 7),
    fantasy_lag_8 = lag(fantasy_points, 8),
    fantasy_lag_9 = lag(fantasy_points, 9),
    fantasy_lag_10 = lag(fantasy_points, 10)
  ) |>
  ungroup()

# Get max round and season
max_round_data <- fantasy_all |> arrange(desc(start_time_utc)) |> slice_head(n = 1)

# Create training and test splits
training <-
  fantasy_all |> 
  filter(!(season_name == max_round_data$season_name[[1]] & round == max_round_data$round[[1]]))

test <-
  fantasy_all |> 
  filter((season_name == max_round_data$season_name[[1]] & round == max_round_data$round[[1]]))

##%######################################################%##
#                                                          #
####                    Add DVP Data                    ####
#                                                          #
##%######################################################%##

# Get DVP for round
get_dvp_round <- function(input_season, input_round) {
# Join with position data
current_data <-
  fantasy_all |> 
  left_join(positions, by = c("player_full_name" = "player_name", "round" = "round", "season_name" = "season"))

# Filter current data to one before input round
current_data <-
  current_data |> 
  filter(season_name <= input_season) |> 
  filter((round < input_round & season_name == input_season) | (season_name < input_season))

# Get list of opposition teams
team_list <-
  current_data |>
  distinct(opposition_team) |> 
  pull(opposition_team)

# Get list of positions
position_list <-
  current_data |>
  distinct(position) |>
  filter(!is.na(position)) |> 
  pull(position)

# Function to get difference between average vs all other teams vs score vs team
get_dvp <- function(opp_team, pos, n_rounds) {
  
  # Get last n rounds data
  rounds_list <-
    current_data |>
    distinct(round, season_name) |>
    arrange(desc(season_name), desc(round)) |>
    slice_head(n = n_rounds) |>
    mutate(round_id = paste(season_name, round))
  
  # Get positional dataset
  data <-
    current_data |>
    mutate(round_id = paste(season_name, round)) |> 
    filter(round_id %in% rounds_list$round_id) |> 
    filter(position == pos)
  
  # Avg vs all other sides
  vs_others <-
    data |>
    filter(opposition_team != opp_team) |>
    summarise(avg_vs_others = mean(fantasy_points, na.rm = TRUE), .by = player_full_name)
  
  # Avg vs side
  vs_team <-
    data |>
    filter(opposition_team == opp_team) |>
    summarise(avg_vs_team = mean(fantasy_points, na.rm = TRUE), .by = player_full_name)
  
  # Join together
  all_data <-
    inner_join(vs_others, vs_team)
  
  # Get dvp score
  score = mean(all_data$avg_vs_team) - mean(all_data$avg_vs_others)
  score = round(score, 2)
  
  # Get percentage scoring over their avg vs team_list
  percentage = mean(all_data$avg_vs_team > all_data$avg_vs_others)
  percentage = round(percentage * 100, 2)
  
  tibble(opposition_team = opp_team, dvp = score, percentage_over_avg = percentage)
}

# Create df of all combinations
dvp_data <-
  expand_grid(opp_team = team_list,
              pos = position_list,
              n_rounds = c(5, 10, 15))

# Apply function to each combination of the dataframe columns
dvp_output <-
  pmap_df(.l = dvp_data, .f = get_dvp) |>
  cbind(dvp_data[,-1])

# Add round number to dvp output
dvp_output$round <- input_round
dvp_output$season <- input_season

return(dvp_output)
  }

# Get list of rounds and seasons to consider
rounds_to_consider <-
  expand_grid(input_round = unique(training$round),
              input_season = unique(training$season_name)) |>
  filter(str_detect(input_round, "Round"))

# Wrap get_dvp_round function with safely
safe_get_dvp_round <- safely(get_dvp_round)

# Apply the safely-wrapped function to each row of the dataframe
results <- pmap(rounds_to_consider, function(...) {
  res <- safe_get_dvp_round(...)
  if (is.null(res$error)) {
    return(res$result)
  } else {
    return(NULL)
  }
})

# Filter out NULL values and bind rows
all_dvp <- bind_rows(discard(results, is.null))

# Filter out all NAN
all_dvp <-
  all_dvp |> 
  filter(!is.nan(dvp))

# Separate DVP into 5, 10 and 15 game datasets
dvp_5 <- all_dvp |> filter(n_rounds == 5) |> rename(dvp_5 = dvp, percentage_over_avg_5 = percentage_over_avg) |> select(-n_rounds)
dvp_10 <- all_dvp |> filter(n_rounds == 10) |> rename(dvp_10 = dvp, percentage_over_avg_10 = percentage_over_avg) |> select(-n_rounds)
dvp_15 <- all_dvp |> filter(n_rounds == 15) |> rename(dvp_15 = dvp, percentage_over_avg_15 = percentage_over_avg) |> select(-n_rounds)

##%######################################################%##
#                                                          #
####                 Get past n average                 ####
#                                                          #
##%######################################################%##

get_last_n_avgs <- function(input_round, input_season) {
  player_data <-
  fantasy_all |> 
    filter(season_name <= input_season) |> 
    filter((round < input_round & season_name == input_season) | (season_name < input_season)) |> 
    arrange(player_full_name, desc(start_time_utc))
  
  last_3_stats <- player_data |> 
    group_by(player_full_name) |> 
    filter(n() >= 3) |>  
    slice_head(n = 3) |>  
    summarise(
      last_3_avg = mean(fantasy_points),
      last_3_min = min(fantasy_points),
      last_3_max = max(fantasy_points),
      last_3_variance = var(fantasy_points)
    )
  
  last_5_stats <- player_data |> 
    group_by(player_full_name) |> 
    filter(n() >= 5) |> 
    slice_head(n = 5) |>  
    summarise(
      last_5_avg = mean(fantasy_points),
      last_5_min = min(fantasy_points),
      last_5_max = max(fantasy_points),
      last_5_variance = var(fantasy_points)
    )
  
  last_7_stats <- player_data |> 
    group_by(player_full_name) |> 
    filter(n() >= 7) |> 
    slice_head(n = 7) |>  
    summarise(
      last_7_avg = mean(fantasy_points),
      last_7_min = min(fantasy_points),
      last_7_max = max(fantasy_points),
      last_7_variance = var(fantasy_points)
    )
  
  last_10_stats <- player_data |> 
    group_by(player_full_name) |> 
    filter(n() >= 10) |> 
    slice_head(n = 10) |>  
    summarise(
      last_10_avg = mean(fantasy_points),
      last_10_min = min(fantasy_points),
      last_10_max = max(fantasy_points),
      last_10_variance = var(fantasy_points)
    )
  
  output <- (last_3_stats |> full_join(last_5_stats) |> full_join(last_7_stats) |> full_join(last_10_stats))
  output$round <- input_round
  output$season <- input_season
  
  return(output)
}

# Wrap get_last_n_avgs function with safely
safe_get_last_n_avgs <- safely(get_last_n_avgs)

# Apply the safely-wrapped function to each row of the dataframe
results <- pmap(rounds_to_consider, function(...) {
  res <- safe_get_last_n_avgs(...)
  if (is.null(res$error)) {
    return(res$result)
  } else {
    return(NULL)
  }
})

# Filter out NULL values and bind rows
last_n_summaries <- bind_rows(discard(results, is.null))

# Filter out all NAN
last_n_summaries <-
  last_n_summaries |> 
  filter(!is.nan(last_3_avg))

##%######################################################%##
#                                                          #
####      Get most common position in last 5 games      ####
#                                                          #
##%######################################################%##

# Add most common position in last 5 games

# Make round number an ordered factor
positions$round <-
  factor(
    positions$round,
    levels = c(
      'Round 1',
      'Round 2',
      'Round 3',
      'Round 4',
      'Round 5',
      'Round 6',
      'Round 7',
      'Round 8',
      'Round 9',
      'Round 10',
      'Round 11',
      'Round 12',
      'Round 13',
      'Round 14',
      'Round 15',
      'Round 16',
      'Round 17',
      'Round 18',
      'Round 19',
      'Round 20',
      'Round 21',
      'Round 22',
      'Round 23',
      'Round 24',
      'Finals Week 1',
      'Semi Finals',
      'Preliminary Finals',
      'Grand Final'
    ),
    ordered = TRUE
  )

# Function to get position
get_position <- function(input_season, input_round) {
  output <-
  positions |> 
    filter(season <= input_season) |> 
    filter((round < input_round & season == input_season) | (season < input_season)) |> 
    arrange(desc(round)) |> 
    group_by(player_name) |> 
    filter(n() >= 5) |> 
    slice_head(n = 5) |> 
    group_by(player_name, position) |> 
    tally() |>
    arrange(player_name, desc(n)) |> 
    group_by(player_name) |> 
    slice_head(n = 1) |> 
    ungroup() |> 
    rename(player_full_name = player_name) |> 
    select(-n)
  
  output$round <- input_round
  output$season <- input_season
  
  return(output)
}

# Wrap get_position function with safely
safe_get_position <- safely(get_position)

# Apply the safely-wrapped function to each row of the dataframe
results <- pmap(rounds_to_consider, function(...) {
  res <- safe_get_position(...)
  if (is.null(res$error)) {
    return(res$result)
  } else {
    return(NULL)
  }
})

# Filter out NULL values and bind rows
last_5_position <- bind_rows(discard(results, is.null))

# Filter out all NAN
last_5_position <-
  last_5_position |> 
  filter(!is.nan(position))

##%######################################################%##
#                                                          #
####             Combine together to create             ####
####             training and test datasets             ####
#                                                          #
##%######################################################%##

#===============================================================================
# Training
#===============================================================================

training <-
training |> 
  mutate(home_away = player_team == home_team) |> 
  select(player_full_name, player_team, opposition_team, margin, round, season = season_name, contains("fantasy"), home_away) |> 
  left_join(last_n_summaries) |> 
  left_join(last_5_position) |> 
  left_join(dvp_5, by = c("season", "round", "opposition_team", "position" = "pos")) |>
  left_join(dvp_10, by = c("season", "round", "opposition_team", "position" = "pos")) |>
  left_join(dvp_15, by = c("season", "round", "opposition_team", "position" = "pos")) |>
  select(-position)

training <- training[complete.cases(training), ]

# Make home_away binary
training <-
  training |> 
  rename(home_game = home_away) |> 
  mutate(home_game = as.numeric(home_game))


#===============================================================================
# Test
#===============================================================================

# round number
test_round <- test$round[[1]]

test <-
  test |>
  mutate(home_away = player_team == home_team) |>
  select(player_full_name,
         player_team,
         opposition_team,
         round,
         season = season_name,
         contains("fantasy"),
         home_away,
         margin) |> 
  left_join(last_n_summaries) |> 
  left_join(last_5_position) |> 
  left_join(dvp_5, by = c("season", "round", "opposition_team", "position" = "pos")) |>
  left_join(dvp_10, by = c("season", "round", "opposition_team", "position" = "pos")) |>
  left_join(dvp_15, by = c("season", "round", "opposition_team", "position" = "pos")) |>
  select(-position)

test <- test[complete.cases(test), ]

# Make home_away binary
test <-
  test |> 
  rename(home_game = home_away) |> 
  mutate(home_game = as.numeric(home_game))

#===============================================================================
# Target Data
#===============================================================================

fixture <- fitzRoy::fetch_fixture_afl()

latest_round_1 <- 
  fixture |>
  filter(compSeason.currentRoundNumber == round.roundNumber) |> 
  mutate(compSeason.name = str_extract(compSeason.name, "\\d+")) |> 
  mutate(season = as.numeric(compSeason.name)) |> 
  select(home_team = home.team.name, away_team = away.team.name, round = round.name, season) |> 
  mutate(player_team = home_team)

latest_round_2 <- 
  fixture |>
  filter(compSeason.currentRoundNumber == round.roundNumber) |> 
  mutate(compSeason.name = str_extract(compSeason.name, "\\d+")) |> 
  mutate(season = as.numeric(compSeason.name)) |> 
  select(home_team = home.team.name, away_team = away.team.name, round = round.name, season) |> 
  mutate(player_team = away_team)

latest_round <-
  bind_rows(latest_round_1, latest_round_2) |> 
  mutate(home_away = player_team == home_team) |> 
  rename(home_game = home_away) |> 
  mutate(home_game = as.numeric(home_game)) |> 
  mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
  select(player_team, opposition_team, round, season, home_game)

# Get players and player team var
target <-
  training |>
  distinct(player_full_name, player_team) |> 
  left_join(latest_round)

# round number
target_round <- target$round[[1]]

target <-
  target |>
  select(player_full_name,
         player_team,
         opposition_team,
         round,
         season,
         home_game) |> 
  left_join(last_n_summaries) |> 
  left_join(last_5_position) |> 
  left_join(dvp_5, by = c("season", "round", "opposition_team", "position" = "pos")) |>
  left_join(dvp_10, by = c("season", "round", "opposition_team", "position" = "pos")) |>
  left_join(dvp_15, by = c("season", "round", "opposition_team", "position" = "pos")) |>
  select(-position)


target <- target[complete.cases(target), ]

# Get 2023 players and teams
player_teams_2023 <-
  training |>
  filter(season == 2023) |>
  transmute(id = paste(player_full_name, player_team))

# Filter
target <-
  target |>
  filter(paste(player_full_name, player_team) %in% player_teams_2023$id)

#===============================================================================
# Write out as CSVs and RDS
#===============================================================================

training |> write_csv("Modelling/Predictive-Models/Data/training_data_fantasy.csv")
test |> write_csv("Modelling/Predictive-Models/Data/test_data_fantasy.csv")
target |> write_csv("Modelling/Predictive-Models/Data/target_data_fantasy.csv")

training |> write_rds("Modelling/Predictive-Models/Data/training_data_fantasy.rds")
test |> write_rds("Modelling/Predictive-Models/Data/test_data_fantasy.rds")
target |> write_rds("Modelling/Predictive-Models/Data/target_data_fantasy.rds")
