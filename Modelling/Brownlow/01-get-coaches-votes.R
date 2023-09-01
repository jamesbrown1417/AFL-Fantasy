library(tidyverse)
library(jsonlite)
library(rvest)
library(fitzRoy)

# Get helper functions
source("Functions/data_processing_functions.r")

# Get data
data_2023 <- get_fantasy_data(season = 2023)
afl_fantasy_2014_2022_data <- readRDS("Data/afl_fantasy_2014_2022_data.rds")

#===============================================================================
# Get 2023 coaches votes
#===============================================================================

# Create function to get data
get_coaches_votes <- function(data_file_path) {

# Read in data
html_data <- read_html(data_file_path)

data_nodes <- html_nodes(html_data, ".col-12.py-2")

# extract nested nodes for names and votes
names <- data_nodes %>% 
  html_nodes(".row.border-bottom.pt-1.pb-1.div-hover") %>% 
  html_nodes(".col-10") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  gsub("\t", "", .)

votes <- data_nodes %>% 
  html_nodes(".row.border-bottom.pt-1.pb-1.div-hover") %>% 
  html_nodes(".col-2.text-center") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  gsub("\t", "", .)

tibble(names, votes, round = data_file_path |> str_extract("\\d+"))
}

# Apply function to list
html_file_list <- list.files("Modelling/Brownlow/coaches_votes_html", full.names = TRUE)

# Get final list
coaches_votes_2023 <-
map(html_file_list, get_coaches_votes) |>
  bind_rows() |>
  mutate(round = as.integer(round), votes = as.integer(votes)) |>
  arrange(round)

# Add team info
coaches_votes_2023 <-
coaches_votes_2023 |> 
  mutate(player_full_name = str_replace(names, " \\(.*\\)", "")) |>
  transmute(player_full_name, coaches_votes = votes, season = "2023", round = paste("Round", round))


#===============================================================================
# Get 2014 - 2022 coaches votes
#===============================================================================

# Coaches Votes-----------------------------------------------------------------
coaches_votes_2014 <- fetch_coaches_votes(season = 2014)
coaches_votes_2015 <- fetch_coaches_votes(season = 2015)
coaches_votes_2016 <- fetch_coaches_votes(season = 2016)
coaches_votes_2017 <- fetch_coaches_votes(season = 2017)
coaches_votes_2018 <- fetch_coaches_votes(season = 2018)
coaches_votes_2019 <- fetch_coaches_votes(season = 2019)
coaches_votes_2020 <- fetch_coaches_votes(season = 2020)
coaches_votes_2021 <- fetch_coaches_votes(season = 2021)
coaches_votes_2022 <- fetch_coaches_votes(season = 2022)

# Add round data----------------------------------------------------------------
coaches_votes_2014_2022 <-
  bind_rows(
    coaches_votes_2014,
    coaches_votes_2015,
    coaches_votes_2016,
    coaches_votes_2017,
    coaches_votes_2018,
    coaches_votes_2019,
    coaches_votes_2020,
    coaches_votes_2021,
    coaches_votes_2022
  )  |> 
  mutate(Player.Name = ifelse(Player.Name == "Josh Kennedy (SYD)", "Josh P. Kennedy", Player.Name)) |>
  mutate(Player.Name = ifelse(Player.Name == "Josh J Kennedy (WCE)", "Josh J. Kennedy", Player.Name)) |>
  mutate(Player.Name = ifelse(Player.Name == "Tom J Lynch (RICH)", "Tom J. Lynch", Player.Name)) |>
  mutate(Player.Name = ifelse(Player.Name == "Tom J Lynch (GCFC)", "Tom J. Lynch", Player.Name)) |>
  mutate(Player.Name = ifelse(Player.Name == "Bailey J Williams (WCE)", "Bailey J. Williams", Player.Name)) |>
  mutate(player_full_name = str_replace(Player.Name, " \\(.*\\)", "")) |>
  transmute(player_full_name, coaches_votes = Coaches.Votes, season = Season, round = paste("Round", Round))

# Filter so round is in regular season
valid_rounds <-
  afl_fantasy_2014_2022_data |>
  filter(str_detect(round, "^Round")) |>
  mutate(round_id = paste(season_name, round)) |>
  distinct(round_id)

coaches_votes_2014_2022 <-
  coaches_votes_2014_2022 |> 
  mutate(round_id = paste(season, round)) |>
  filter(round_id %in% valid_rounds$round_id) |>
  select(-round_id) |>
  tibble() |>
  mutate(coaches_votes = as.integer(coaches_votes)) |>
  mutate(season = as.character(season))

#===============================================================================
# Combine and output
#===============================================================================

coaches_votes_2014_2023 <-
  bind_rows(coaches_votes_2014_2022, coaches_votes_2023) |>
  mutate(player_full_name = str_remove_all(player_full_name, "\\r")) |> 
  write_rds("Modelling/Brownlow/coaches_votes_2014_to_2023.rds")
