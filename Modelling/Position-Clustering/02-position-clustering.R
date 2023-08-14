# Use UMAP to cluster the positions

# Load libraries
library(tidyverse)
library(fitzRoy)
library(umap)
library(cluster)
library(factoextra)

# Get player stats data from different sources
afl_stats_2021 <- fetch_player_stats_afl(season =  2021)
afl_stats_2022 <- fetch_player_stats_afl(season =  2022)
afl_stats_2023 <- fetch_player_stats_afl(season =  2023)

afl_stats_all <- bind_rows(afl_stats_2021, afl_stats_2022, afl_stats_2023)

# Using AFL stats for now, select the variables deemed relevant
afl_stats_2023_clustering <-
  afl_stats_all |>
    filter(timeOnGroundPercentage >= 60) |>
    select(
    player_first_name = player.player.player.givenName,
    player_last_name = player.player.player.surname,
    kicks,
    marks,
    handballs,
    goals,
    behinds,
    hitouts,
    tackles,
    rebounds = rebound50s,
    inside_fifties = inside50s,
    contested_possessions = contestedPossessions,
    uncontested_possessions = uncontestedPossessions,
    marks_inside_fifty = marksInside50,
    one_percenters = onePercenters,
    bounces,
    goal_assists = goalAssists,
    # time_on_ground_percentage,
    centre_clearances = clearances.centreClearances,
    stoppage_clearances = clearances.stoppageClearances,
    score_involvements = scoreInvolvements,
    metres_gained = metresGained,
    intercepts,
    tackles_inside_fifty = tacklesInside50,
    contest_def_losses = extendedStats.contestDefLosses,
    contest_def_one_on_ones = extendedStats.contestDefOneOnOnes,
    contest_off_one_on_ones = extendedStats.contestOffOneOnOnes,
    contest_off_wins = extendedStats.contestOffWins,
    def_half_pressure_acts = extendedStats.defHalfPressureActs,
    f50_ground_ball_gets = extendedStats.f50GroundBallGets,
    ground_ball_gets = extendedStats.groundBallGets,
    intercept_marks = extendedStats.interceptMarks,
    marks_on_lead = extendedStats.marksOnLead,
    pressure_acts = extendedStats.pressureActs,
    ruck_contests = extendedStats.ruckContests,
    score_launches= extendedStats.scoreLaunches,
    shots_at_goal = shotsAtGoal,
    spoils = extendedStats.spoils,
    kickins = extendedStats.kickins,
    centre_bounces = extendedStats.centreBounceAttendances
    ) |>
    mutate(player_name = paste(player_first_name, player_last_name, sep = " ")) |>
    select(-player_first_name, -player_last_name) |>
    relocate(player_name, .before = kicks)

# Normalise the predictors
afl_stats <-
  afl_stats_2023_clustering |>
    mutate_at(vars(-player_name), ~(. - mean(.)) / sd(.))

# Get match details for stats table
match_details <-
  afl_stats_all |>
    mutate(season = year(utcStartTime)) |>
    filter(timeOnGroundPercentage >= 60) |>
    select(home_team = home.team.name,
     away_team = away.team.name,
     season,
     round = round.roundNumber
      ) |>
      mutate(round = as.integer(round))

#===============================================================================
# Create umap object
#===============================================================================

# Create labels and dataframe
afl_stats.data <- as.data.frame(afl_stats[, -1])
afl_stats.labels <- as.data.frame(afl_stats[, 1])

# Perform UMAP
afl_stats.umap <- umap(afl_stats.data, n_neighbors = 15, n_components = 5, metric = "euclidean", min_dist = 0.1, spread = 1)

# Get the UMAP coordinates
afl_stats.umap.coords <- as.data.frame(afl_stats.umap$layout)

#===============================================================================
# Perform K-means clustering to get 8 clusters
#===============================================================================

# Determine optimal number of clusters
# fviz_nbclust(afl_stats.umap.coords, kmeans, method = "wss")
# fviz_nbclust(afl_stats.umap.coords, kmeans, method = "silhouette")

# Above we see ~7 is the optimal number of clusters

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
filter(season == 2023) |> 
filter(round == 21) |>
distinct(player_name)

# Cluster 2
cluster_2 <-
afl_positions.df |>
filter(position == 2) |>
filter(season == 2023) |> 
filter(round == 21) |>
distinct(player_name)

# Cluster 3
cluster_3 <-
  afl_positions.df |>
  filter(position == 3) |>
  filter(season == 2023) |> 
  filter(round == 21) |>
  distinct(player_name)

# Cluster 4
cluster_4 <-
  afl_positions.df |>
  filter(position == 4) |>
  filter(season == 2023) |> 
  filter(round == 21) |>
  distinct(player_name)

# Cluster 5
cluster_5 <-
  afl_positions.df |>
  filter(position == 5) |>
  filter(season == 2023) |> 
  filter(round == 21) |>
  distinct(player_name)

# Cluster 6
cluster_6 <-
  afl_positions.df |>
  filter(position == 6) |>
  filter(season == 2023) |> 
  filter(round == 21) |>
  distinct(player_name)

# Cluster 7
cluster_7 <-
  afl_positions.df |>
  filter(position == 7) |>
  filter(season == 2023) |> 
  filter(round == 21) |>
  distinct(player_name)

# Add cluster names to dataframe
afl_positions.df <-
  afl_positions.df |>
  mutate(position_name = case_when(
    position == 1 ~ "Inside MID",
    position == 2 ~ "Key DEF",
    position == 3 ~ "Outside MID",
    position == 4 ~ "Ruck",
    position == 5 ~ "Gen FWD",
    position == 6 ~ "Gen DEF",
    position == 7 ~ "Key FWD"
  ))

# Output to data folder
afl_positions.df |> 
relocate(position, .before = position_name) |>
  write_rds("Data/afl_clustering_positions.rds")
