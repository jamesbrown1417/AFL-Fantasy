# Libraries and functions
library(tidyverse)
library(fitzRoy)

`%notin%` <- Negate(`%in%`)

# Load data processing functions
source("Functions/data_processing_functions.R")

# Get Data
current_season_data <- get_fantasy_data(season = Sys.Date() |> year())

# Get player positions from clustering algorithm
positions <-
 read_rds("Data/afl_clustering_positions.rds") |>
 mutate(round = paste("Round", round)) |>
 select(player_name, round, position)

# Join with position data
current_season_data <-
current_season_data |> 
  left_join(positions, by = c("player_full_name" = "player_name", "round" = "round"))

# Make round number an ordered factor
current_season_data$round <-
  factor(
    current_season_data$round,
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

# Get list of opposition teams
team_list <-
current_season_data |>
  distinct(opposition_team) |> 
  pull(opposition_team)

# Get list of positions
position_list <-
current_season_data |>
  distinct(position) |>
  filter(!is.na(position)) |> 
  pull(position)

# Function to get difference between average vs all other teams vs score vs team
get_dvp <- function(opp_team, pos, n_rounds) {
  
  # Get last n rounds data
  rounds_list <-
  current_season_data |>
  distinct(round) |>
  arrange(desc(round)) |>
  slice_head(n = n_rounds)

  # Get positional dataset
  data <-
    current_season_data |>
    filter(round %in% rounds_list$round) |>
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

# Function to get difference between average vs all other teams vs score vs team
get_dvp_disposals <- function(opp_team, pos, n_rounds) {
  
  # Get last n rounds data
  rounds_list <-
  current_season_data |>
  distinct(round) |>
  arrange(desc(round)) |>
  slice_head(n = n_rounds)

  # Get positional dataset
  data <-
    current_season_data |>
    filter(round %in% rounds_list$round) |>
    filter(position == pos)
  
  # Avg vs all other sides
  vs_others <-
    data |>
    filter(opposition_team != opp_team) |>
    summarise(avg_vs_others = mean(disposals, na.rm = TRUE), .by = player_full_name)
  
  # Avg vs side
  vs_team <-
    data |>
    filter(opposition_team == opp_team) |>
    summarise(avg_vs_team = mean(disposals, na.rm = TRUE), .by = player_full_name)
  
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

# Apply function to each combination of the dataframe columns
dvp_output_disposals <-
 pmap_df(.l = dvp_data, .f = get_dvp_disposals) |>
 cbind(dvp_data[,-1])

 #==========================================================#
 # Write out to Data Folder
 #==========================================================#

# Combine
dvp_output_all <-
bind_rows(
    dvp_output_disposals |> mutate(type = "Disposals"),
    dvp_output |> mutate(type = "Fantasy Points")
)

# Write out
write_rds(dvp_output_all, "Data/DVP_data.rds")