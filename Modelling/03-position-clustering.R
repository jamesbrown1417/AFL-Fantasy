# Use UMAP to cluster the positions

# Load libraries
library(tidyverse)
library(fitzRoy)
library(umap)
library(cluster)
library(factoextra)

# Get player stats data from different sources
afl_stats_2023 <- fetch_player_stats_afl(season = 2023)
fryzigg_stats_2023 <- fetch_player_stats_fryzigg(season = 2023)

# Using fryzigg stats for now, select the variables deemed relevant
fryzigg_stats_2023_clustering <-
    fryzigg_stats_2023 |>
    filter(time_on_ground_percentage >= 60) |>
    select(player_first_name,
    player_last_name,
    kicks,
    marks,
    handballs,
    effective_disposals,
    goals,
    behinds,
    hitouts,
    tackles,
    rebounds,
    inside_fifties,
    clearances,
    clangers,
    contested_possessions,
    uncontested_possessions,
    marks_inside_fifty,
    one_percenters,
    bounces,
    goal_assists,
    # time_on_ground_percentage,
    centre_clearances,
    stoppage_clearances,
    score_involvements,
    metres_gained,
    intercepts,
    tackles_inside_fifty,
    contest_def_losses,
    contest_def_one_on_ones,
    contest_off_one_on_ones,
    contest_off_wins,
    def_half_pressure_acts,
    f50_ground_ball_gets,
    ground_ball_gets,
    intercept_marks,
    marks_on_lead,
    pressure_acts,
    ruck_contests,
    score_launches,
    shots_at_goal,
    spoils) |>
    mutate(player_name = paste(player_first_name, player_last_name, sep = " ")) |>
    select(-player_first_name, -player_last_name) |>
    relocate(player_name, .before = )

# Normalise the predictors
afl_stats <-
    fryzigg_stats_2023_clustering |>
    mutate_at(vars(-player_name), ~(. - mean(.)) / sd(.))

# Get match details for stats table
match_details <-
fryzigg_stats_2023 |>
    filter(time_on_ground_percentage >= 60) |>
    select(home_team = match_home_team,
     away_team = match_away_team,
     round = match_round
      ) |>
      mutate(round = as.integer(round))


#===============================================================================
# Create umap object
#===============================================================================

# Create labels and dataframe
afl_stats.data <- as.data.frame(afl_stats[, -1])
afl_stats.labels <- as.data.frame(afl_stats[, 1])

# Perform UMAP
afl_stats.umap <- umap(afl_stats.data, n_neighbors = 15, n_components = 4, metric = "euclidean", min_dist = 0.1, spread = 1)

# Get the UMAP coordinates
afl_stats.umap.coords <- as.data.frame(afl_stats.umap$layout)

#===============================================================================
# Perform K-means clustering to get 8 clusters
#===============================================================================

# Determine optimal number of clusters
fviz_nbclust(afl_stats.umap.coords, kmeans, method = "wss")
fviz_nbclust(afl_stats.umap.coords, kmeans, method = "silhouette")

# Above we see 5 is the optimal number of clusters

# Perform K-means clustering on optimal clusters
afl_positions <- kmeans(afl_stats.umap.coords, centers = 7, nstart = 25)

# Get dataset of labels and clusters
afl_positions.df <- as_tibble(cbind(afl_stats.labels, afl_positions$cluster, match_details)) |>
rename(position = `afl_positions$cluster`)

#===============================================================================
# Get the players in each cluster
#===============================================================================

# Cluster 1
cluster_1 <-
afl_positions.df |>
filter(position == 1) |>
filter(round == 18) |>
distinct(player_name)

# Cluster 2
cluster_2 <-
afl_positions.df |>
filter(position == 2) |>
filter(round == 18) |>
distinct(player_name)

# Cluster 3
cluster_3 <-
  afl_positions.df |>
  filter(position == 3) |>
  filter(round == 18) |>
  distinct(player_name)

# Cluster 4
cluster_4 <-
  afl_positions.df |>
  filter(position == 4) |>
  filter(round == 18) |>
  distinct(player_name)

# Cluster 5
cluster_5 <-
  afl_positions.df |>
  filter(position == 5) |>
  filter(round == 18) |>
  distinct(player_name)

# Cluster 6
cluster_6 <-
  afl_positions.df |>
  filter(position == 6) |>
  filter(round == 18) |>
  distinct(player_name)

# Cluster 7
cluster_7 <-
  afl_positions.df |>
  filter(position == 7) |>
  filter(round == 18) |>
  distinct(player_name)

# Create names for clusters
cluster_names <-
list("Gen FWD" = 1,
     "Key FWD" = 2,
     "Ruck" = 3,
     "Key DEF" = 4,
     "Outside Mid" = 5,
     "Inside Mid" = 6,
     "Gen DEF" = 7
    )

# Add cluster names to dataframe
afl_positions.df <-
  afl_positions.df |>
  mutate(position_name = case_when(
    position == 1 ~ "Gen FWD",
    position == 2 ~ "Key FWD",
    position == 3 ~ "Ruck",
    position == 4 ~ "Key DEF",
    position == 5 ~ "Outside Mid",
    position == 6 ~ "Inside Mid",
    position == 7 ~ "Gen DEF"
  ))

# Output to data folder
afl_positions.df |> 
relocate(position, .before = position_name) |>
  write_rds("Data/afl_clustering_positions.rds")