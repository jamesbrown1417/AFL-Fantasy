# Load libraries
library(tidyverse)
library(fitzRoy)
`%notin%` = base::Negate(`%in%`)

##%######################################################%##
#                                                          #
####                      Get data                      ####
#                                                          #
##%######################################################%##

# Get player details------------------------------------------------------------
player_details_2014 <- fetch_player_details_afl(season = 2014)
player_details_2015 <- fetch_player_details_afl(season = 2015)
player_details_2016 <- fetch_player_details_afl(season = 2016)
player_details_2017 <- fetch_player_details_afl(season = 2017)
player_details_2018 <- fetch_player_details_afl(season = 2018)
player_details_2019 <- fetch_player_details_afl(season = 2019)
player_details_2020 <- fetch_player_details_afl(season = 2020)
player_details_2021 <- fetch_player_details_afl(season = 2021)
player_details_2022 <- fetch_player_details_afl(season = 2022)
player_details_2023 <- fetch_player_details_afl(season = 2023)

player_details_all <-
  bind_rows(
    player_details_2014,
    player_details_2015,
    player_details_2016,
    player_details_2017,
    player_details_2018,
    player_details_2019,
    player_details_2020,
    player_details_2021,
    player_details_2022,
    player_details_2023
  )

player_details_all <-
  player_details_all |> 
  mutate(player_name = paste(firstName, surname)) |>
  select(season, player_name, player_team = team)

player_details_all <-
  player_details_all |> 
  mutate(player_name = ifelse(player_name == "Josh Kennedy" & player_team == "West Coast Eagles", "Josh J. Kennedy", player_name)) |>
  mutate(player_name = ifelse(player_name == "Josh Kennedy" & player_team == "Sydney Swans", "Josh P. Kennedy", player_name)) |>
  mutate(player_name = ifelse(player_name == "Callum Brown" & player_team == "Collingwood", "Callum L. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Callum Brown" & player_team == "GWS Giants", "Callum J. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Bailey Williams" & player_team == "West Coast Eagles", "Bailey J. Williams", player_name)) |>
  mutate(player_name = ifelse(player_name == "Nathan Brown" & player_team %in% c("Collingwood", "St Kilda"), "Nathan J. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Sam Reid" & player_team == "GWS Giants", "Sam J. Reid", player_name)) |>
  mutate(player_name = ifelse(player_name == "Scott Thompson" & player_team == "North Melbourne", "Scott D. Thompson", player_name)) |>
  mutate(player_name = ifelse(player_name == "Tom Lynch" & player_team %in% c("Richmond", "Gold Coast Suns"), "Tom J. Lynch", player_name))

# Get player stats--------------------------------------------------------------
afl_stats_2014 <- fetch_player_stats_afl(season = 2014)
afl_stats_2015 <- fetch_player_stats_afl(season = 2015)
afl_stats_2016 <- fetch_player_stats_afl(season = 2016)
afl_stats_2017 <- fetch_player_stats_afl(season = 2017)
afl_stats_2018 <- fetch_player_stats_afl(season = 2018)
afl_stats_2019 <- fetch_player_stats_afl(season = 2019)
afl_stats_2020 <- fetch_player_stats_afl(season = 2020)
afl_stats_2021 <- fetch_player_stats_afl(season = 2021)
afl_stats_2022 <- fetch_player_stats_afl(season = 2022)
afl_stats_2023 <- fetch_player_stats_afl(season = 2023)

afl_stats <-
  bind_rows(
    afl_stats_2014,
    afl_stats_2015,
    afl_stats_2016,
    afl_stats_2017,
    afl_stats_2018,
    afl_stats_2019,
    afl_stats_2020,
    afl_stats_2021,
    afl_stats_2022,
    afl_stats_2023
  )

# Get Brownlow votes------------------------------------------------------------
brownlow_votes_2014 <- fetch_player_stats_afltables(season = 2014)
brownlow_votes_2015 <- fetch_player_stats_afltables(season = 2015)
brownlow_votes_2016 <- fetch_player_stats_afltables(season = 2016)
brownlow_votes_2017 <- fetch_player_stats_afltables(season = 2017)
brownlow_votes_2018 <- fetch_player_stats_afltables(season = 2018)
brownlow_votes_2019 <- fetch_player_stats_afltables(season = 2019)
brownlow_votes_2020 <- fetch_player_stats_afltables(season = 2020)
brownlow_votes_2021 <- fetch_player_stats_afltables(season = 2021)
brownlow_votes_2022 <- fetch_player_stats_afltables(season = 2022)

# Combine
brownlow_votes <-
  bind_rows(
    brownlow_votes_2014,
    brownlow_votes_2015,
    brownlow_votes_2016,
    brownlow_votes_2017,
    brownlow_votes_2018,
    brownlow_votes_2019,
    brownlow_votes_2020,
    brownlow_votes_2021,
    brownlow_votes_2022
  )

# Fix round
brownlow_votes$Round <- paste("Round", brownlow_votes$Round)

# Create player_name variable
brownlow_votes <-
  brownlow_votes |>
  transmute(
    match = paste(Home.team, "v", Away.team),
    Season = as.integer(Season),
    Round,
    player_name = paste(First.name, Surname),
    jumper_number = Jumper.No.,
    player_team = Playing.for,
    brownlow_votes = Brownlow.Votes,
    kicks = Kicks,
    marks = Marks,
    handballs = Handballs,
    goals = Goals
  )

# Fix team names
brownlow_votes <-
  brownlow_votes |> 
  mutate(match = str_replace_all(match, "Adelaide", "Adelaide Crows")) |>
  mutate(match = str_replace_all(match, "Geelong", "Geelong Cats")) |>
  mutate(match = str_replace_all(match, "Gold Coast", "Gold Coast Suns")) |>
  mutate(match = str_replace_all(match, "Greater Western Sydney", "GWS Giants")) |>
  mutate(match = str_replace_all(match, "^Sydney", "Sydney Swans")) |>
  mutate(match = str_replace_all(match, "West Coast", "West Coast Eagles")) |>
  
  mutate(player_team = str_replace_all(player_team, "Adelaide", "Adelaide Crows")) |>
  mutate(player_team = str_replace_all(player_team, "Geelong", "Geelong Cats")) |>
  mutate(player_team = str_replace_all(player_team, "Gold Coast", "Gold Coast Suns")) |>
  mutate(player_team = str_replace_all(player_team, "Greater Western Sydney", "GWS Giants")) |>
  mutate(player_team = str_replace_all(player_team, "^Sydney", "Sydney Swans")) |>
  mutate(player_team = str_replace_all(player_team, "West Coast", "West Coast Eagles"))
  
##%######################################################%##
#                                                          #
####             Read in coaches votes data             ####
#                                                          #
##%######################################################%##

coaches_votes <-
  read_rds("Modelling/Brownlow/coaches_votes_2014_to_2023.rds") |>
  mutate(season = as.integer(season)) |>
  rename(player_name = player_full_name) |>
  rename(Season = season, Round = round)

##%######################################################%##
#                                                          #
#### select the variables deemed relevant for modelling ####
#                                                          #
##%######################################################%##

afl_stats_brownlow <-
  afl_stats |>
  mutate(match = paste(home.team.name, "v", away.team.name)) |>
  select(
    match,
    player_first_name = player.player.player.givenName,
    player_last_name = player.player.player.surname,
    start_time = utcStartTime,
    Round = round.name,
    kicks,
    marks,
    handballs,
    goals,
    behinds,
    hitouts,
    tackles,
    disposal_efficiency = disposalEfficiency,
    rebounds = rebound50s,
    inside_fifties = inside50s,
    contested_possessions = contestedPossessions,
    uncontested_possessions = uncontestedPossessions,
    marks_inside_fifty = marksInside50,
    one_percenters = onePercenters,
    bounces,
    goal_assists = goalAssists,
    time_on_ground_percentage = timeOnGroundPercentage,
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
    turnovers,
    rating_points = ratingPoints,
    centre_bounces = extendedStats.centreBounceAttendances
  ) |>
  mutate(player_name = paste(player_first_name, player_last_name, sep = " ")) |>
  select(-player_first_name, -player_last_name) |>
  relocate(player_name, .after = Round) |>
  filter(time_on_ground_percentage >= 40)

# Add player team
afl_stats_brownlow <-
  afl_stats_brownlow |> 
  mutate(season = year(start_time)) |>
  left_join(player_details_all, by = c("season", "player_name"),
            relationship = "many-to-many")

##%######################################################%##
#                                                          #
####        Get supercoach points from footywire        ####
#                                                          #
##%######################################################%##

# Individual dataframes
sc_2014 <- fetch_player_stats_footywire(season = 2014)
sc_2015 <- fetch_player_stats_footywire(season = 2015)
sc_2016 <- fetch_player_stats_footywire(season = 2016)
sc_2017 <- fetch_player_stats_footywire(season = 2017)
sc_2018 <- fetch_player_stats_footywire(season = 2018)
sc_2019 <- fetch_player_stats_footywire(season = 2019)
sc_2020 <- fetch_player_stats_footywire(season = 2020)
sc_2021 <- fetch_player_stats_footywire(season = 2021)
sc_2022 <- fetch_player_stats_footywire(season = 2022)
sc_2023 <- fetch_player_stats_footywire(season = 2023)

# Combine and select required variables
supercoach_all <-
  bind_rows(
    sc_2014,
    sc_2015,
    sc_2016,
    sc_2017,
    sc_2018,
    sc_2019,
    sc_2020,
    sc_2021,
    sc_2022,
    sc_2023
  ) |>
  select(
    season = Season,
    round = Round,
    player_name = Player,
    player_team = Team,
    kicks = K,
    handballs = HB,
    marks = M,
    goals = G,
    opposition_team = Opposition,
    supercoach = SC
  )

# Fix team names
supercoach_all <-
  supercoach_all |> 
  mutate(player_team = str_replace_all(player_team, "Adelaide", "Adelaide Crows")) |>
  mutate(player_team = str_replace_all(player_team, "Brisbane", "Brisbane Lions")) |>
  mutate(player_team = str_replace_all(player_team, "Geelong", "Geelong Cats")) |>
  mutate(player_team = str_replace_all(player_team, "Gold Coast", "Gold Coast Suns")) |>
  mutate(player_team = str_replace_all(player_team, "GWS", "GWS Giants")) |>
  mutate(player_team = str_replace_all(player_team, "^Sydney", "Sydney Swans")) |>
  mutate(player_team = str_replace_all(player_team, "West Coast", "West Coast Eagles"))

##%######################################################%##
#                                                          #
####   Fix names for footywire and afltables sources    ####
#                                                          #
##%######################################################%##

# Fix initial players for duplicate names players
supercoach_all <-
supercoach_all |>
  mutate(player_name = ifelse(player_name == "Joshua Kennedy" & player_team == "West Coast Eagles", "Josh J. Kennedy", player_name)) |>
  mutate(player_name = ifelse(player_name == "Callum Brown" & player_team == "Collingwood", "Callum L. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Callum Brown" & player_team == "GWS Giants", "Callum M. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Bailey Williams" & player_team == "West Coast Eagles", "Bailey J. Williams", player_name)) |>
  mutate(player_name = ifelse(player_name == "Nathan Brown" & player_team %in% c("Collingwood", "St Kilda"), "Nathan J. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Samuel Reid" & player_team == "GWS Giants", "Sam J. Reid", player_name)) |>
  mutate(player_name = ifelse(player_name == "Scott Thompson" & player_team == "North Melbourne", "Scott D. Thompson", player_name)) |>
  mutate(player_name = ifelse(player_name == "Jason H-Francis", "Jason Horne-Francis", player_name)) |>
  mutate(player_name = ifelse(player_name == "Nasiah W-Milera", "Nasiah Wanganeen-Milera", player_name)) |>
  mutate(player_name = ifelse(player_name == "Tom Lynch" & player_team %in% c("Richmond", "Gold Coast"), "Tom J. Lynch", player_name))

brownlow_votes <-
  brownlow_votes |> 
  mutate(player_name = ifelse(player_name == "Josh Kennedy" & player_team == "West Coast Eagles", "Josh J. Kennedy", player_name)) |>
  mutate(player_name = ifelse(player_name == "Josh Kennedy" & player_team == "Sydney Swans", "Josh P. Kennedy", player_name)) |>
  mutate(player_name = ifelse(player_name == "Callum Brown" & player_team == "Collingwood", "Callum L. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Callum Brown" & player_team == "GWS Giants", "Callum M. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Bailey Williams" & player_team == "West Coast Eagles", "Bailey J. Williams", player_name)) |>
  mutate(player_name = ifelse(player_name == "Nathan Brown" & player_team %in% c("Collingwood", "St Kilda"), "Nathan J. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Sam Reid" & player_team == "GWS Giants", "Sam J. Reid", player_name)) |>
  mutate(player_name = ifelse(player_name == "Scott Thompson" & player_team == "North Melbourne", "Scott D. Thompson", player_name)) |>
  mutate(player_name = ifelse(player_name == "Tom Lynch" & player_team %in% c("Richmond", "Gold Coast Suns"), "Tom J. Lynch", player_name))
  
afl_stats_brownlow <-
  afl_stats_brownlow |> 
  mutate(player_name = ifelse(player_name == "Josh Kennedy" & player_team == "West Coast Eagles", "Josh J. Kennedy", player_name)) |>
  mutate(player_name = ifelse(player_name == "Josh Kennedy" & player_team == "Sydney Swans", "Josh P. Kennedy", player_name)) |>
  mutate(player_name = ifelse(player_name == "Callum Brown" & player_team == "Collingwood", "Callum L. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Callum Brown" & player_team == "GWS Giants", "Callum M. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Bailey Williams" & player_team == "West Coast Eagles", "Bailey J. Williams", player_name)) |>
  mutate(player_name = ifelse(player_name == "Nathan Brown" & player_team %in% c("Collingwood", "St Kilda"), "Nathan J. Brown", player_name)) |>
  mutate(player_name = ifelse(player_name == "Sam Reid" & player_team == "GWS Giants", "Sam J. Reid", player_name)) |>
  mutate(player_name = ifelse(player_name == "Scott Thompson" & player_team == "North Melbourne", "Scott D. Thompson", player_name)) |>
  mutate(player_name = ifelse(player_name == "Tom Lynch" & player_team %in% c("Richmond", "Gold Coast Suns"), "Tom J. Lynch", player_name))

# Make player names title case
supercoach_all$player_name <- str_to_title(supercoach_all$player_name)
afl_stats_brownlow$player_name <- str_to_title(afl_stats_brownlow$player_name)
brownlow_votes$player_name <- str_to_title(brownlow_votes$player_name)

# Get Map of names
names_afl <-
  afl_stats_brownlow |>
  mutate(season = year(start_time)) |>
  select(season,
         round = Round,
         player_name_afl = player_name,
         kicks,
         handballs,
         marks,
         goals)

names_afltables <-
  brownlow_votes |>
  select(season = Season,
         round = Round,
         player_name_afltables = player_name,
         kicks,
         handballs,
         marks,
         goals)

names_footywire <-
  supercoach_all |> 
  select(season,
         round,
         player_name_footywire = player_name,
         kicks,
         handballs,
         marks,
         goals)

names_map <-
  names_afl |>
  full_join(names_afltables) |>
  full_join(names_footywire) |> 
  group_by(player_name_afl, player_name_afltables, player_name_footywire) |>
  tally() |>
  arrange(player_name_afl, desc(n)) |>
  group_by(player_name_afl) |>
  slice_head(n = 1) |>
  filter(!is.na(player_name_afltables))

##%######################################################%##
#                                                          #
####              Standardise player names              ####
#                                                          #
##%######################################################%##

# afltables
brownlow_votes <-
  brownlow_votes |> 
  left_join(names_map |> select(player_name_afl, player_name_afltables), by = c("player_name" = "player_name_afltables")) |>
  mutate(player_name = coalesce(player_name_afl, player_name)) |>
  select(-player_name_afl)

# footywire
supercoach_all <-
  supercoach_all |> 
  left_join(names_map |> select(player_name_afl, player_name_footywire), by = c("player_name" = "player_name_footywire")) |>
  mutate(player_name = coalesce(player_name_afl, player_name)) |>
  select(-player_name_afl)

##%######################################################%##
#                                                          #
####                 Get match margins                  ####
#                                                          #
##%######################################################%##

# Individual Seasons
results_2014 <- fetch_results_afl(season = 2014)
results_2015 <- fetch_results_afl(season = 2015)
results_2016 <- fetch_results_afl(season = 2016)
results_2017 <- fetch_results_afl(season = 2017)
results_2018 <- fetch_results_afl(season = 2018)
results_2019 <- fetch_results_afl(season = 2019)
results_2020 <- fetch_results_afl(season = 2020)
results_2021 <- fetch_results_afl(season = 2021)
results_2022 <- fetch_results_afl(season = 2022)
results_2023 <- fetch_results_afl(season = 2023)

# Combine
results_all <-
bind_rows(
  results_2014,
  results_2015,
  results_2016,
  results_2017,
  results_2018,
  results_2019,
  results_2020,
  results_2021,
  results_2022,
  results_2023
)

# Tidy
results_all <-
  results_all |> 
  transmute(
    Season = as.double(round.year),
    Round = round.name,
    match = str_replace(match.name, "Vs", "v"),
         home_team = match.homeTeam.name,
         away_team = match.awayTeam.name,
         margin = homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore)

##%######################################################%##
#                                                          #
####         Add extra variables to main table          ####
#                                                          #
##%######################################################%##

afl_stats_brownlow_analysis <-
  afl_stats_brownlow |>
  mutate(Season = as.integer(year(start_time))) |>
  left_join(brownlow_votes |> transmute(Season, Round, player_name, brownlow_votes)) |>
  left_join(supercoach_all |> transmute(Season = as.integer(season), Round = round, player_name, supercoach)) |> 
  left_join(results_all |> mutate(Season = as.integer(Season))) |> 
  relocate(player_team, home_team, away_team, margin, .after = match)

##%######################################################%##
#                                                          #
####         Normalise variables by match total         ####
#                                                          #
##%######################################################%##

afl_stats_brownlow_analysis <-
  afl_stats_brownlow_analysis |>
  mutate(margin = ifelse(player_team == home_team, margin, -1*margin)) |> 
  relocate(player_name, match, Season, Round, .before = start_time) |>
  left_join(brownlow_votes) |>
  left_join(coaches_votes) |>
  relocate(brownlow_votes, coaches_votes, .after = Round) |>
  group_by(Season, Round, match) |>
  mutate(kicks = kicks / sum(kicks, na.rm = TRUE),
         marks = marks / sum(marks, na.rm = TRUE),
         handballs = handballs / sum(handballs, na.rm = TRUE),
         goals = goals / sum(goals, na.rm = TRUE),
         hitouts = hitouts / sum(hitouts, na.rm = TRUE),
         tackles = tackles / sum(tackles, na.rm = TRUE),
         disposal_efficiency,
         rebounds = rebounds / sum(rebounds, na.rm = TRUE),
         inside_fifties = inside_fifties / sum(inside_fifties, na.rm = TRUE),
         contested_possessions = contested_possessions / sum(contested_possessions, na.rm = TRUE),
         uncontested_possessions = uncontested_possessions / sum(uncontested_possessions, na.rm = TRUE),
         marks_inside_fifty = marks_inside_fifty / sum(marks_inside_fifty, na.rm = TRUE),
         one_percenters = one_percenters / sum(one_percenters, na.rm = TRUE),
         centre_clearances = centre_clearances / sum(centre_clearances, na.rm = TRUE),
         stoppage_clearances = stoppage_clearances / sum(stoppage_clearances, na.rm = TRUE),
         score_involvements = score_involvements / sum(score_involvements, na.rm = TRUE),
         metres_gained,
         intercepts = intercepts / sum(intercepts, na.rm = TRUE),
         ground_ball_gets = ground_ball_gets / sum(ground_ball_gets, na.rm = TRUE),
         intercept_marks = intercept_marks / sum(intercept_marks, na.rm = TRUE),
         pressure_acts = pressure_acts / sum(pressure_acts, na.rm = TRUE),
         score_launches = score_launches / sum(score_launches, na.rm = TRUE),
         turnovers = turnovers / sum(turnovers, na.rm = TRUE),
         rating_points,
         supercoach = supercoach / sum(supercoach, na.rm = TRUE)) |>
  ungroup() |>
  select(-kickins, -centre_bounces, -jumper_number)

# Filter to only regular season rounds
afl_stats_brownlow_analysis <-
  afl_stats_brownlow_analysis |>
  filter(
    Round %in% c(
      "Round 1",
      "Round 2",
      "Round 3",
      "Round 4",
      "Round 5",
      "Round 6",
      "Round 7",
      "Round 8",
      "Round 9",
      "Round 10",
      "Round 11",
      "Round 12",
      "Round 13",
      "Round 14",
      "Round 15",
      "Round 16",
      "Round 17",
      "Round 18",
      "Round 19",
      "Round 20",
      "Round 21",
      "Round 22",
      "Round 23",
      "Round 24"
    )
  )

##%######################################################%##
#                                                          #
####                  Write out as RDS                  ####
#                                                          #
##%######################################################%##

afl_stats_brownlow_analysis |>
  write_rds("Modelling/Brownlow/brownlow_analysis_data.rds")
