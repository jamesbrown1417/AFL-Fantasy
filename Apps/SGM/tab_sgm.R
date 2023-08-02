library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(mongolite)
uri <- Sys.getenv("mongodb_connection_string")

# TAB SGM-----------------------------------------------------------------------
tabsgm_con <- mongo(collection = "TAB-SGM", db = "Odds", url = uri)
tab_sgm <- tabsgm_con$find('{}') |> tibble()

#==============================================================================
# Function to get SGM data
#===============================================================================

# Function to get SGM data
get_sgm_tab <- function(data, player_names, disposal_counts) {
  if (length(player_names) != length(disposal_counts)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>% 
      filter(player_name == player_names[i] &
               number_of_disposals == disposal_counts[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  # Get the 'id' column as a list
  id_list <- filtered_df$id
  
  # Create the propositions list using the id_list
  propositions <- lapply(id_list, function(id) list(type = unbox("WIN"), propositionId = unbox(id)))
  
  return(propositions)
}

#==============================================================================
# Make Post Request
#==============================================================================

# Make Post Request
call_sgm_tab <- function(data, player_names, disposal_counts) {
  tryCatch({
    if (length(player_names) != length(disposal_counts)) {
      stop("Both lists should have the same length")
    }
    
    filtered_df <- data.frame()
    for (i in seq_along(player_names)) {
      temp_df <- data %>% 
        filter(player_name == player_names[i] &
                 number_of_disposals == disposal_counts[i])
      filtered_df <- bind_rows(filtered_df, temp_df)
    }
    
    # Unadjusted price
    unadjusted_price <- prod(filtered_df$price)
    
    # Get propositions
    propositions <- get_sgm_tab(data, player_names, disposal_counts)
    
    url <- "https://api.beta.tab.com.au/v1/pricing-service/enquiry"
    
    headers <- c("Content-Type" = "application/json")
    
    payload <- list(
      clientDetails = list(jurisdiction = unbox("SA"), channel = unbox("web")),
      bets = list(
        list(
          type = unbox("FIXED_ODDS"),
          legs = list(
            list(
              type = unbox("SAME_GAME_MULTI"),
              propositions = propositions
            )
          )
        )
      )
    )
    
    response <- POST(url, body = toJSON(payload), add_headers(.headers = headers), encode = "json")
    
    if (http_error(response)) {
      stop("HTTP request failed. Please check your URL or network connection.")
    }
    
    response_content <- content(response, "parsed")
    adjusted_price <- as.numeric(response_content$bets[[1]]$legs[[1]]$odds$decimal)
    adjustment_factor <- adjusted_price / unadjusted_price
    combined_list <- paste(player_names, disposal_counts, sep = ": ")
    player_string <- paste(combined_list, collapse = ", ")
    
    output_data <- data.frame(
      Selections = player_string,
      Unadjusted_Price = unadjusted_price,
      Adjusted_Price = adjusted_price,
      Adjustment_Factor = adjustment_factor,
      Agency = 'TAB'
    )
    
    return(output_data)
  }, error = function(e) {
    print(paste("Error: ", e))
  })
}
