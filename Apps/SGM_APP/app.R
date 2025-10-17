#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(readxl)  
`%notin%` <- Negate(`%in%`)

# AFL-specific DVP and position normalization removed for NBL

# Head to head data (used for match ordering)
h2h <- read_csv("../../Data/scraped_odds/sportsbet_h2h.csv")

# Matches in order
matches_in_order <-
  h2h %>%
  distinct(match) |> 
  pull()

#===============================================================================
# Create compare sgm function
#===============================================================================

# Source scripts
source("betright_sgm.R")
source("tab_sgm.R")
source("sportsbet_sgm.R")
source("pointsbet_sgm.R")
source("bet365_sgm.R")
source("dabble_sgm.R")
## player_combos.R not used in NBL SGM

#===============================================================================
# Create compare sgm function
#===============================================================================

compare_sgm <- function(player_names, stat_counts, markets, types) {
  # Function to handle errors in the call_sgm functions
  handle_call_sgm <- function(func, sgm, player_names, stat_counts, markets, types) {
    tryCatch({
      func(sgm, player_names, stat_counts, markets, types)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections=NA, Markets = NA, Unadjusted_Price=NA, Adjusted_Price=NA, Adjustment_Factor=NA, Agency=NA)
    })
  }
  
  # Get individual dataframes
  pointsbet_data <- handle_call_sgm(call_sgm_pointsbet, pointsbet_sgm, player_names, stat_counts, markets, types)
  sportsbet_data <- handle_call_sgm(call_sgm_sportsbet, sportsbet_sgm, player_names, stat_counts, markets, types)
  tab_data <- handle_call_sgm(call_sgm_tab, tab_sgm, player_names, stat_counts, markets, types)
  betright_data <- handle_call_sgm(call_sgm_betright, betright_sgm, player_names, stat_counts, markets, types)
  bet365_data <- handle_call_sgm(call_sgm_bet365, bet365_sgm, player_names, stat_counts, markets, types)
  dabble_data <- handle_call_sgm(call_sgm_dabble, dabble_sgm, player_names, stat_counts, markets, types)
  
  # Bind together and return
  bind_rows(pointsbet_data, sportsbet_data, tab_data, betright_data, bet365_data, dabble_data) |>
    mutate(Adjusted_Price = round(Adjusted_Price, 2),
           Unadjusted_Price = round(Unadjusted_Price, 2),
           Adjustment_Factor = round(Adjustment_Factor, 2)
           ) |>
    arrange(desc(Adjusted_Price))
}

#===============================================================================
# Compare CGM function
#===============================================================================

compare_cgm <- function(player_names_cross, lines_cross, market_names_cross, types_cross) {
  # List of each agency data
  all_data <- list(pointsbet_sgm, sportsbet_sgm, tab_sgm, betright_sgm, bet365_sgm, dabble_sgm)
  
  # Function to get cross game multi data
  get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross, types_cross) {
    if (length(player_names_cross) != length(lines_cross) || length(lines_cross) != length(market_names_cross)) {
      stop("All lists should have the same length")
    }
    
    filtered_df <- data.frame()
    for (i in seq_along(player_names_cross)) {
      temp_df <- data %>%
        filter(player_name == player_names_cross[i],
               line == lines_cross[i],
               market_name == market_names_cross[i],
               type == types_cross[i])
      filtered_df <- bind_rows(filtered_df, temp_df)
    }
    
    if (nrow(filtered_df) != length(player_names_cross)) {
      return(NULL)
    }
    
    price <- prod(filtered_df$price)
    
    combined_list <- paste(player_names_cross, lines_cross, sep = ": ")
    player_string <- paste(combined_list, collapse = ", ")
    market_string <- paste(market_names_cross, collapse = ", ")
    match_string <- paste(filtered_df$match, collapse = ", ")
    
    output_data <- data.frame(
      Selections = player_string,
      Matches = match_string,
      Markets = market_string,
      Price = round(price, 2),
      Agency = first(data$agency)
    )
    
    return(output_data)
  }
  
  # Function to handle errors in the get_cgm function
  handle_get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross, types_cross) {
    tryCatch({
      get_cgm(data, player_names_cross, lines_cross, market_names_cross, types_cross)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections = NA, Matches = NA, Markets = NA, Price = NA, Agency = NA)
    })
  }
  
  # Map over list of dataframes
  cgm_all <- map_dfr(all_data, handle_get_cgm, player_names_cross, lines_cross, market_names_cross, types_cross) %>%
    arrange(desc(Price))
  
  return(cgm_all)
}

# Read in NBL processed odds datasets (Points/Rebounds/Assists/Threes)---------
points <- read_rds("../../Data/processed_odds/all_player_points.rds")
rebounds <- read_rds("../../Data/processed_odds/all_player_rebounds.rds")
assists <- read_rds("../../Data/processed_odds/all_player_assists.rds")
threes <- read_rds("../../Data/processed_odds/all_player_threes.rds")

# Combine NBL props then expand to Over/Under rows with unified fields
props_all <- bind_rows(points, rebounds, assists, threes)

overs <- props_all |>
  mutate(type = "Over",
         price = over_price,
         prob_2025 = empirical_prob_over_2024_25,
         diff_2025 = diff_over_2024_25,
         prob_last_10 = empirical_prob_over_last_10,
         diff_last_10 = diff_over_last_10)

unders <- props_all |>
  filter(!is.na(under_price)) |>
  mutate(type = "Under",
         price = under_price,
         prob_2025 = empirical_prob_under_2024_25,
         diff_2025 = diff_under_2024_25,
         prob_last_10 = empirical_prob_under_last_10,
         diff_last_10 = diff_under_last_10)

disposals <- bind_rows(overs, unders)

# Create market best
disposals <-
  disposals |>
  group_by(match, player_name, market_name, line, type) |>
  arrange(desc(price), .by_group = TRUE) |>
  mutate(
    max_player_diff = max(diff_last_10, na.rm = TRUE),
    second_best_price = if_else(n() >= 2, nth(price, 2), NA_real_),
    market_best = row_number() == 1
  ) |>
  ungroup()

# Unique matches (prefer H2H order; fallback to props if H2H empty)
matches_from_props <-
  disposals |>
  filter(!is.na(match)) |>
  distinct(match) |>
  pull() |> 
  unique()

if (length(matches_in_order) == 0) {
  matches <- matches_from_props
} else {
  # Use H2H order but include any matches present only in props
  extra_matches <- setdiff(matches_from_props, matches_in_order)
  matches <- c(matches_in_order, extra_matches)
}

# Unique agencies
agencies <-
  disposals |>
  distinct(agency) |>
  pull() |> 
  unique()

# Create disposals dataframe to display
disposals_display <-
  disposals |>
  group_by(player_name, match, line, market_name) |>
  mutate(
    next_best_diff = if_else(market_best,
                            ((1/second_best_price) - (1/price)),
                             NA_real_)
  ) |>
  ungroup() |>
  arrange(desc(max_player_diff)) |>
  transmute(match,
         player_name,
         type,
         market_name,
         line,
         price,
         agency,
         prob_2025 = round(prob_2025, 2),
         diff_2025 = round(diff_2025, 2),
         prob_last_10 = round(prob_last_10, 2),
         diff_last_10 = round(diff_last_10, 2),
         next_best_diff = round(100 * next_best_diff, 1),
         market_best)


##%######################################################%##
#                                                          #
####                         UI                         ####
#                                                          #
##%######################################################%##

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("NBL SGM"),
  
  tabsetPanel(
    
    # SGM Tab
    tabPanel("SGM",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "match",
                   "Select Match",
                   choices = matches,
                   selected = NULL
                 ),
                 selectInput(
                   "agency",
                   "Select Agency",
                   choices = agencies,
                   selected = NULL
                 ),
                 selectInput(
                   "market",
                   "Select Market",
                    choices = c("Player Points", "Player Rebounds", "Player Assists", "Player Threes"),
                    selected = c("Player Points", "Player Rebounds", "Player Assists", "Player Threes"),
                    multiple = TRUE
                  ),
                 checkboxInput("best_odds", "Only Show Best Market Odds?", value = FALSE),
                 h3("Selections"),
                 DT::dataTableOutput("selected"),
                 h3("SGM Information"),
                 uiOutput("summary"),
                 h3("Odds Comparison"),
                 actionButton("get_comparison", label = "Compare Odds"),
                 actionButton("clear_comparison", label = "Clear Selections"),
                 DT::dataTableOutput("odds_compare")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Player List", 
                            DT::dataTableOutput("table")
                   )
                   )
               )
             )
    ),
    
    # Cross Game Multi Tab
    tabPanel("Cross Game Multi",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "agency_cross",
                   "Select Agency",
                   choices = agencies,
                   selected = NULL
                 ),
                 selectInput(
                   "market_cross",
                   "Select Market",
                    choices = c("Player Points", "Player Rebounds", "Player Assists", "Player Threes"),
                    selected = c("Player Points", "Player Rebounds", "Player Assists", "Player Threes"),
                    multiple = TRUE
                  ),
                 checkboxInput("best_odds_cross", "Only Show Best Market Odds?", value = FALSE),
                 h3("Selections"),
                 DT::dataTableOutput("selected_cross"),
                 h3("Multi Information"),
                 uiOutput("summary_cross"),
                 actionButton("get_comparison_cross", label = "Compare Odds"),
                 actionButton("clear_comparison_cross", label = "Clear Selections"),
                 DT::dataTableOutput("odds_compare_cross")
               ),
               
               mainPanel(
                 DT::dataTableOutput("table_cross")
               )
             )
    )
  )
)

##%######################################################%##
#                                                          #
####                       Server                       ####
#                                                          #
##%######################################################%##

server <- function(input, output, session) {
  
  # For the "SGM" panel
  output$table <- renderDT({
    filtered_data <-
      disposals_display[disposals_display$match == input$match &
                          disposals_display$agency == input$agency &
                          disposals_display$market_name %in% input$market,]

    if (input$best_odds) {
      filtered_data <- filtered_data |>
        filter(market_best) |>
        select(-market_best)
    } else {
      filtered_data <- filtered_data |> select(-next_best_diff)
    }
    
    datatable(filtered_data, selection = "multiple", filter = "top")
  }, server = FALSE) # We are setting this as FALSE for client-side processing of the DataTable
  
  
  observeEvent(input$table_rows_selected,{
    output$selected <- renderDT({
      if(!is.null(input$table_rows_selected)){
        filtered_data <-
          disposals_display[disposals_display$match == input$match &
                              disposals_display$agency == input$agency &
                              disposals_display$market_name %in% input$market,]
        
        if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
        selected_data <- filtered_data[input$table_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data)
      }
    })
  })
  
  # Get the table proxy
  proxy <- dataTableProxy("table")
  
  # Get the table proxy for the cross game multi
  proxy_cross <- dataTableProxy("table_cross")
  
  # Pairwise correlations removed for NBL adaptation
  
  # SGM Comparison
  observeEvent(input$get_comparison, {
    # Get selected data
    filtered_data <- disposals_display[disposals_display$match == input$match &
                                         disposals_display$agency == input$agency &
                                         disposals_display$market_name %in% input$market,]
    
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "type", "line", "market_name", "price")]
    
    player_names = selected_data$player_name
    types = selected_data$type
    lines = selected_data$line
    market_names = selected_data$market_name
    
    # Call function
    comparison_df <- compare_sgm(
      player_names = player_names,
      stat_counts = lines,
      markets = market_names,
      types = types)
    
    # populate DTOutput
    output$odds_compare <- renderDT({
      datatable(comparison_df)
    })
  })
  
  # Observe the click event on the "clear_rows" button
  observeEvent(input$clear_comparison, {
    # Deselect all rows in the table
    selectRows(proxy, NULL)
  })
  
  observeEvent(input$clear_comparison_cross, {
    # Deselect all rows in the table
    selectRows(proxy_cross, NULL)
  })
  
  output$summary <- renderUI({
    if(!is.null(input$table_rows_selected)){
      filtered_data <- disposals_display[disposals_display$match == input$match &
                                           disposals_display$agency == input$agency &
                                           disposals_display$market_name %in% input$market,]
      
      if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
      selected_data <- filtered_data[input$table_rows_selected, ]
      uncorrelated_price <- prod(selected_data$price)
      empirical_price <- 1 / prod(selected_data$prob_last_10)
      HTML(paste0("<strong>Uncorrelated Price:</strong>", " $", round(uncorrelated_price, 2), "<br/>",
                  " <strong>Theoretical Uncorrelated Price:</strong>", " $", round(empirical_price, 2)))
    }
  })
  
  # For the "Cross Game Multi" panel
  output$table_cross <- renderDT({
    filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                               disposals_display$market_name %in% input$market_cross,]
    
    if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
    
    datatable(filtered_data_cross, selection = "multiple", filter = "top")
  }, server = FALSE) 
  
  observeEvent(input$table_cross_rows_selected,{
    output$selected_cross <- renderDT({
      if(!is.null(input$table_cross_rows_selected)){
        filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                                 disposals_display$market_name %in% input$market_cross,]
        
        if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
        
        selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data_cross)
      }
    })
  })
  
  # Cross Game Comparison
  observeEvent(input$get_comparison_cross, {
    # Get selected data
    filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                               disposals_display$market_name %in% input$market_cross,]
    
    if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
    
    selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, c("player_name", "type", "line", "market_name", "price", "agency")]
    
    player_names_cross = selected_data_cross$player_name
    types_cross = selected_data_cross$type
    lines_cross = selected_data_cross$line
    market_names_cross = selected_data_cross$market_name
    
    # Call function
    comparison_df_cross <-
      compare_cgm(market_names_cross = market_names_cross,
                  player_names_cross = player_names_cross,
                  lines_cross = lines_cross,
                  types_cross = types_cross)
    
    # populate DTOutput
    output$odds_compare_cross <- renderDT({
      datatable(comparison_df_cross)
    })
  })
  
  output$summary_cross <- renderUI({
    if(!is.null(input$table_cross_rows_selected)){
      filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                               disposals_display$market_name %in% input$market_cross,]
      
      if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
      
      selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, ]
      uncorrelated_price_cross <- prod(selected_data_cross$price)
      empirical_price_cross <- 1 / prod(selected_data_cross$prob_2025)
      empirical_price_cross_l10 <- 1 / prod(selected_data_cross$prob_last_10)
      diff = 1/empirical_price_cross - 1/uncorrelated_price_cross
      diff_l10 = 1/empirical_price_cross_l10 - 1/uncorrelated_price_cross
      HTML(paste0("<strong>Multi Price:</strong>", " $", round(uncorrelated_price_cross, 2), "<br/>",
                  " <strong>Theoretical Multi Price:</strong>", " $", round(empirical_price_cross, 2), "<br/>",
                  " <strong>Edge L10:</strong>", " ", round(100*diff_l10, 3), "%"), "<br/>",
                  " <strong>Edge 2025:</strong>", " ", round(100*diff, 3), "%")
    }
  })
  
  # Player Combos functionality removed
}

##%######################################################%##
#                                                          #
####                      Run App                       ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
