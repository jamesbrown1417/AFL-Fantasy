#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(readxl)
`%notin%` <- Negate(`%in%`)


# Read in data
disposals <- read_excel("../disposals.xlsx")
fantasy <- read_excel("../fantasy.xlsx")
number_of_games <- read_rds("../number_of_games.rds")

# Unique matches
matches <-
  disposals |>
  arrange(start_time) |> 
  filter(start_time > now()) |> 
  distinct(match) |>
  pull()
  
# Unique agencies
agencies <-
  disposals |>
  distinct(agency) |>
  pull()

# Create disposals dataframe to display
disposals_display <-
  disposals |>
  arrange(desc(max_player_diff)) |> 
  transmute(match,
         player_name,
         number_of_disposals,
         price,
         agency,
         prob_2023 = round(empirical_probability_2023, 2),
         prob_last_7 = round(empirical_probability_last_7, 2),
         diff_2023 = round(diff_2023, 2),
         diff_last_7 = round(diff_last_7, 2)) |> 
  left_join(number_of_games)

# Get correlations
correlations_2023 <-
  read_rds("../../Data/player_correlations_disposals_23.rds") |> 
  mutate_if(is.numeric, round, digits = 2)

# Get matchup difficulties

##%######################################################%##
#                                                          #
####                         UI                         ####
#                                                          #
##%######################################################%##

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("match", "Select Match", choices = matches, selected = NULL),
      selectInput("agency", "Select Agency", choices = agencies, selected = NULL),
      h3("Selections"),  # Header in sidebar
      DTOutput("selected"),  # DataTable in sidebar
      h3("Pairwise Correlations"),
      DTOutput(outputId = "correlations"),
      h3("SGM Information"),
      uiOutput(outputId = "summary")
    ),
    
    mainPanel(
      DTOutput("table")
    )
  )
)

##%######################################################%##
#                                                          #
####                       Server                       ####
#                                                          #
##%######################################################%##

server <- function(input, output, session) {
  
  output$table <- renderDT({
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    datatable(filtered_data, selection = "multiple")
  }, server = FALSE) # We are setting this as FALSE for client-side processing of the DataTable
  
  observeEvent(input$table_rows_selected,{
    output$selected <- renderDT({
      if(!is.null(input$table_rows_selected)){
        filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
        selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
        datatable(selected_data)
      }
    })
  })
  
  output$correlations <- renderDT({
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
    
    correlations_table <- correlations_2023 |> filter(player_a %in% selected_data$player_name & player_b %in% selected_data$player_name)
    datatable(correlations_table)
  })
  
  output$summary <- renderUI({
    if(!is.null(input$table_rows_selected)){
      filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
      selected_data <- filtered_data[input$table_rows_selected, ]
      uncorrelated_price <- prod(selected_data$price)
      empirical_price <- 1 / prod(selected_data$prob_2023)
      HTML(paste0("<strong>Uncorrelated Price:</strong>", " $", round(uncorrelated_price, 2), "<br/>",
                 " <strong>Empirical Uncorrelated Price:</strong>", " $", round(empirical_price, 2)))
    }
  })
  
  
  
}

##%######################################################%##
#                                                          #
####                      Run App                       ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
