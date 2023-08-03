library(tidyverse)
library(jsonlite)
library(rvest)
library(fitzRoy)

# Get helper functions
source("Functions/data_processing_functions.r")

# Get data
data_2023 <- get_fantasy_data(season = 2023)
afl_fantasy_2014_2022_data <- readRDS("../../Data/afl_fantasy_2014_2022_data.rds")

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

#===============================================================================
# Get 2014 - 2022 coaches votes
#===============================================================================


