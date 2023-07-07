##%######################################################%##
#                                                          #
####                       Set up                       ####
#                                                          #
##%######################################################%##

# Libraries---------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)

# Data--------------------------------------------------------------------------
combined_stats_table <- read_rds("combined_stats_table.rds")

# Player names
player_names <-
combined_stats_table |>
    mutate(player_full_name = paste(first_name, family_name)) |> 
    summarise(total_points = sum(player_points, na.rm = TRUE), .by = player_full_name) |> 
    arrange(desc(total_points)) |> 
    filter(player_full_name != "NA NA") |>
    distinct(player_full_name) |> 
    pull(player_full_name)

# Create function to filter and display player data
display_player_stats <- function(player_name, season_name, n_games) {
    combined_stats_table |>
        mutate(player_full_name = paste(first_name, family_name)) |>
        filter(player_full_name == player_name) |>
        filter(season == season_name) |>
        arrange(desc(match_time_utc)) |> 
        select(player_full_name,
               season,
               round_number,
               team = name,
               opposition = opp_name,
               home_away,
               starter,
               player_points,
               player_rebounds_total,
               player_assists,
               player_steals,
               player_blocks,
               player_minutes
               ) |> 
        slice_head(n = n_games)
}

# Function to plot hit rate for prop lines
display_empirical_probabilities <-
    function(data, stat, line) {
        # Create Label variable
        if (stat == "player_points") {
            label = "Points"
        }
        else if (stat == "player_rebounds") {
            label = "Rebounds"
        }
        else {
            label = "Assists"
        }
        
        # Stat as symbol
        stat <- rlang::sym(stat)
        
        # number of games
        num_games = nrow(data)
        
        # Create data
        dat <- data %>%
            mutate(over_line = if_else(!!stat >= line, TRUE, FALSE)) %>%
            mutate(num_games_ago = row_number())
        
        # Hit rate vars
        hit_rate = paste0(round(mean(dat$over_line) * 100, 2), "%")
        
        # Player name var
        name_var = dat$player_full_name[1]
        
        # Create plot
        dat %>%
            ggplot(aes(
                x = num_games_ago,
                y = !!stat,
                color = over_line
            )) +
            geom_point(alpha = 0.8, size = 4) +
            geom_line(aes(group = 1), color = "black") +
            geom_hline(
                yintercept = line,
                linetype = "dashed",
                color = "black",
                linewidth = 1.5
            ) +
            scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
            scale_x_reverse(breaks = seq(num_games, 0,-1)) +
            labs(
                title = paste0("Player Performance: ", name_var),
                subtitle = paste0("Hit Rate: ", hit_rate),
                x = "Number of Games Ago",
                y = label,
                color = "Over Line"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5),
                legend.position = "bottom",
                legend.title = element_text(face = "bold")
            )
    }

##%######################################################%##
#                                                          #
####                        App                         ####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####                       UI                            ####
#                                                          #
##%######################################################%##
ui <- fluidPage(titlePanel("Player Performance Analysis"),
                theme = shinytheme("united"),
    sidebarLayout(
        sidebarPanel(
            selectInput("player_name", "Enter Player's Name", choices = player_names),
            selectInput(
                "season_name",
                "Season",
                choices = c(
                    "2022-2023",
                    "2021-2022",
                    "2020-2021",
                    "2019-2020",
                    "2018-2019",
                    "2017-2018",
                    "2016-2017",
                    "2015-2016"
                )
            ),
            numericInput(
                "n_games",
                "Number of Games to Display",
                value = 5,
                min = 1
            ),
            selectInput(
                "stat",
                "Statistic to Display",
                choices = list(
                    "Points" = "player_points",
                    "Rebounds" = "player_rebounds_total",
                    "Assists" = "player_assists"
                ),
                selected = "player_points"
            ),
            numericInput("line", "Set Prop Line", value = 20, min = 0),
            
            h3("Player Stats Plot"),
            plotOutput("player_stats_plot", height = "400px")
        ),
        
        mainPanel(h3("Player Stats Table"),
                  dataTableOutput("player_stats_table"))
    )
)

##%######################################################%##
#                                                          #
####                   Server logic                     ####
#                                                          #
##%######################################################%##

server <- function(input, output) {
    
    output$player_stats_table <- renderDataTable({
        display_player_stats(input$player_name, input$season_name, input$n_games)
    })
    
    output$player_stats_plot <- renderPlot({
        data <- display_player_stats(input$player_name, input$season_name, input$n_games)
        display_empirical_probabilities(data, input$stat, input$line)
    })
    
}

##%######################################################%##
#                                                          #
####                       Run                          ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
