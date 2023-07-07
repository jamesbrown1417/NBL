##%######################################################%##
#                                                          #
####              Scrape the 2023-2024 NBL              ####
####           season schedule from wikipedia           ####
#                                                          #
##%######################################################%##

#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(rvest)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Scrape data
#===============================================================================

# URL of data
data_url = "https://en.wikipedia.org/wiki/2023%E2%80%9324_NBL_season"

# Scrape data
season_schedule <-
read_html(data_url) |>
    html_elements(".vevent") |> 
    rvest::html_table(trim = TRUE) |> 
    bind_rows() |> 
    filter(X3 == "vs.") |> 
    transmute(match_date = dmy(X1),
              match = paste(X2, X3, X4),
              home_team = X2,
              away_team = X4,
              venue = X5
              )

#===============================================================================
# Write data
#===============================================================================

# Make team names consistent----------------------------------------------------

season_schedule <-
    season_schedule |>
    mutate(home_team = str_replace_all(
        home_team,
        c("S.E. Melbourne Phoenix" = "South East Melbourne Phoenix")
    )) |> 
    mutate(away_team = str_replace_all(
        away_team,
        c("S.E. Melbourne Phoenix" = "South East Melbourne Phoenix")
    )) 
    
# Write out
season_schedule |> 
    write_rds("Data/season_schedule_2023_2024.rds")
