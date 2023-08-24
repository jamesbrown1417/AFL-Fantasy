# Use UMAP to cluster the positions

# Load libraries
library(tidyverse)
library(fitzRoy)
library(umap)
library(cluster)
library(factoextra)
library(dbscan)

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

# # Create labels and dataframe
afl_stats.data <- as.data.frame(afl_stats[, -1])
afl_stats.labels <- as.data.frame(afl_stats[, 1])


#===============================================================================
# Create umap object - possible problems with umap -> clustering (leave for now)
#===============================================================================

# # Perform UMAP
# afl_stats.umap <- umap(afl_stats.data, n_neighbors = 30, n_components = 4, metric = "euclidean", min_dist = 0.001, spread = 1)
# 
# # Get the UMAP coordinates
# afl_stats.umap.coords <- as.data.frame(afl_stats.umap$layout)

#===============================================================================
# Dimensionality reduction using PCA
#===============================================================================

# Get numeric matrix
afl_stats_matrix <- 
  afl_stats |>
  select(-player_name) |>
  as.matrix()

# Perform PCA
pca_result <- prcomp(afl_stats_matrix)

# Get PCA Scores
pc_scores <- pca_result$x

# Decide how many to keep
plot(pca_result$sdev^2, type='b', xlab="Component", ylab="Eigenvalue") # Keep 5 PCs

# Get retained PCA Scores
pc_scores_retained <- pc_scores[,1:5]

#===============================================================================
# Perform K-means clustering to get clusters
#===============================================================================

# Perform K-means clustering on optimal clusters
afl_positions <- kmeans(pc_scores_retained, centers = 7, nstart = 35)

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
filter(round == 22) |>
distinct(player_name)

# Cluster 2
cluster_2 <-
afl_positions.df |>
filter(position == 2) |>
filter(season == 2023) |> 
filter(round == 22) |>
distinct(player_name)

# Cluster 3
cluster_3 <-
  afl_positions.df |>
  filter(position == 3) |>
  filter(season == 2023) |> 
  filter(round == 22) |>
  distinct(player_name)

# Cluster 4
cluster_4 <-
  afl_positions.df |>
  filter(position == 4) |>
  filter(season == 2023) |> 
  filter(round == 22) |>
  distinct(player_name)

# Cluster 5
cluster_5 <-
  afl_positions.df |>
  filter(position == 5) |>
  filter(season == 2023) |> 
  filter(round == 22) |>
  distinct(player_name)

# Cluster 6
cluster_6 <-
  afl_positions.df |>
  filter(position == 6) |>
  filter(season == 2023) |> 
  filter(round == 22) |>
  distinct(player_name)

# Cluster 7
cluster_7 <-
  afl_positions.df |>
  filter(position == 7) |>
  filter(season == 2023) |> 
  filter(round == 22) |>
  distinct(player_name)

# Output to data folder
afl_positions.df |> 
  write_rds("Data/afl_clustering_positions.rds")
