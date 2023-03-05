# Get all afl fantasy data since 2014 and save as an RDS file:

# Source functions
source("Functions/data_processing_functions.R")

# Libraries and functions
library(purrr)

# Vector of years
years = 2014:2022

# Apply function to years
afl_fantasy_2014_2022_data <-
  map(years, get_fantasy_data)

# Bind together the tibbles
afl_fantasy_2014_2022_data <-
  afl_fantasy_2014_2022_data |> 
  reduce(dplyr::bind_rows)

# Output as an RDS object
saveRDS(afl_fantasy_2014_2022_data, "Data/afl_fantasy_2014_2022_data.rds")