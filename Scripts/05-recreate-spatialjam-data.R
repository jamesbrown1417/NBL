#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in Data
#===============================================================================

combined_stats_table <-
    read_rds("Data/combined_stats_table.rds") |> 
    filter(season == "2025-2026")

#===============================================================================
# Get opposition team data
#===============================================================================

opposition_team_data <-
combined_stats_table |> 
    select(match_id,
           name = opp_name,
           contains("match"))

names(opposition_team_data) <- str_replace(names(opposition_team_data), "match", "opp")

opposition_team_data <-
    opposition_team_data |> 
    rename(match_id = opp_id) |> 
    select(-opp_time_utc, -opp_minutes) |> 
    distinct(match_id, name, .keep_all = TRUE)

# Add opposition team data to combined_stats_table
combined_stats_table <-
    combined_stats_table |> 
    left_join(opposition_team_data,
              by = c("match_id", "name"))

#===============================================================================
# 1.) Player Averages
#===============================================================================

player_averages <-
combined_stats_table |> 
    group_by(first_name, family_name, name) |> 
    mutate(minutes_played = period_to_seconds(ms(player_minutes)) / 60) |> 
    filter(minutes_played >= 5) |> 
    summarise(GP = n(),
              MINS = mean(minutes_played),
              `2PA` = mean(player_two_pointers_attempted),
              `2PM` = mean(player_two_pointers_made),
              `3PA` = mean(player_three_pointers_attempted),
              `3PM` = mean(player_three_pointers_made),
              FGA = mean(player_field_goals_attempted),
              FGM = mean(player_field_goals_made),
              FTA = mean(player_free_throws_attempted),
              FTM = mean(player_free_throws_made),
              dREB = mean(player_rebounds_defensive),
              oREB = mean(player_rebounds_offensive),
              tREB = mean(player_rebounds_total),
              AST = mean(player_assists),
              STL = mean(player_steals),
              BLK = mean(player_blocks),
              PF = mean(player_fouls_personal),
              PFV = mean(player_fouls_received),
              TO = mean(player_turnovers),
              PTS = mean(player_points)) |> 
    arrange(desc(`AST`)) |>
    mutate(`2P%` = round(`2PM` / `2PA`, 3),
           `3P%` = round(`3PM` / `3PA`, 3),
           `FG%` = round(FGM / FGA, 3),
           `FT%` = round(FTM / FTA, 3)) |>
    relocate(`2P%`, .after = `2PM`) |>
    relocate(`3P%`, .after = `3PM`) |>
    relocate(`FG%`, .after = FGM) |>
    relocate(`FT%`, .after = FTM) |> 
    mutate(across(where(is.double), ~ round(., 2)))

#===============================================================================
# 2.) Player Metrics
#===============================================================================

# Get metrics
player_metrics <-
    combined_stats_table |> 
    mutate(team_minutes = 200 + extra_periods_used*25) |> 
    mutate(minutes_played = period_to_seconds(ms(player_minutes)) / 60) |> 
    filter(minutes_played >= 5) |> 
    mutate(`eFG%` = (player_two_pointers_made + 1.5 * player_three_pointers_made) / player_field_goals_attempted,
           `TS%` = player_points / (2 * (player_field_goals_attempted + 0.44 * player_free_throws_attempted)),
           `PPS` = player_points / player_field_goals_attempted,
           `TOV%` = player_turnovers / (player_field_goals_attempted + 0.44 * player_free_throws_attempted + player_turnovers),
           `AST%` = player_assists / (((minutes_played / (team_minutes / 5)) * match_field_goals_made) - player_field_goals_made),
           `FTR` = player_free_throws_attempted / player_field_goals_attempted,
           `3PAr` = player_three_pointers_attempted / player_field_goals_attempted,
           `dRB%` = (player_rebounds_defensive * (team_minutes / 5)) / (minutes_played * (match_rebounds_defensive + opp_rebounds_offensive)),
           `oRB%` = (player_rebounds_offensive * (team_minutes / 5)) / (minutes_played * (match_rebounds_offensive + opp_rebounds_defensive)),
           `USG%` = ((player_field_goals_attempted + 0.44 * player_free_throws_attempted + player_turnovers) * (team_minutes / 5)) / (minutes_played * (match_field_goals_attempted + 0.44 * match_free_throws_attempted + match_turnovers)))

# Get average for player over the season
player_metrics <-
    player_metrics |> 
    group_by(first_name, family_name, name) |>
    summarise(GP = n(),
              MINS = mean(minutes_played),
              `eFG%` = mean(`eFG%`),
              `TS%` = mean(`TS%`),
              `PPS` = mean(`PPS`),
              `TOV%` = mean(`TOV%`),
              `AST%` = mean(`AST%`),
              `FTR` = mean(`FTR`),
              `3PAr` = mean(`3PAr`),
              `dRB%` = mean(`dRB%`),
              `oRB%` = mean(`oRB%`),
              `USG%` = mean(`USG%`))

#===============================================================================
# 3.) Team Averages
#===============================================================================

team_averages <-
    combined_stats_table |> 
    distinct(match_id, name, .keep_all = TRUE) |>
    mutate(win = if_else(full_score > opp_full_score, 1, 0)) |>
    group_by(name) |>
    summarise(GP = n(),
              wins = sum(win),
              `2PA` = mean(match_two_pointers_attempted),
              `2PM` = mean(match_two_pointers_made),
              `3PA` = mean(match_three_pointers_attempted),
              `3PM` = mean(match_three_pointers_made),
              FGA = mean(match_field_goals_attempted),
              FGM = mean(match_field_goals_made),
              FTA = mean(match_free_throws_attempted),
              FTM = mean(match_free_throws_made),
              dREB = mean(match_rebounds_defensive),
              oREB = mean(match_rebounds_offensive),
              tREB = mean(match_rebounds_total),
              AST = mean(match_assists),
              STL = mean(match_steals),
              BLK = mean(match_blocks),
              PF = mean(match_fouls_personal),
              TOV = mean(match_turnovers),
              PTS = mean(full_score)
              ) |> 
    mutate(`2P%` = round(`2PM` / `2PA`, 3),
           `3P%` = round(`3PM` / `3PA`, 3),
           `FG%` = round(FGM / FGA, 3),
           `FT%` = round(FTM / FTA, 3)) |>
    relocate(`2P%`, .after = `2PM`) |>
    relocate(`3P%`, .after = `3PM`) |>
    relocate(`FG%`, .after = FGM) |>
    relocate(`FT%`, .after = FTM) |> 
    rename(team = name)

#===============================================================================
# 4.) Opposition Averages
#===============================================================================

opposition_averages <-
    combined_stats_table |> 
    distinct(match_id, name, .keep_all = TRUE) |>
    mutate(win = if_else(full_score > opp_full_score, 1, 0)) |>
    group_by(opp_name) |>
    summarise(GP = n(),
              wins = sum(win),
              `2PA` = mean(match_two_pointers_attempted),
              `2PM` = mean(match_two_pointers_made),
              `3PA` = mean(match_three_pointers_attempted),
              `3PM` = mean(match_three_pointers_made),
              FGA = mean(match_field_goals_attempted),
              FGM = mean(match_field_goals_made),
              FTA = mean(match_free_throws_attempted),
              FTM = mean(match_free_throws_made),
              dREB = mean(match_rebounds_defensive),
              oREB = mean(match_rebounds_offensive),
              tREB = mean(match_rebounds_total),
              AST = mean(match_assists),
              STL = mean(match_steals),
              BLK = mean(match_blocks),
              PF = mean(match_fouls_personal),
              TOV = mean(match_turnovers),
              PTS = mean(full_score)
    ) |> 
    mutate(`2P%` = round(`2PM` / `2PA`, 3),
           `3P%` = round(`3PM` / `3PA`, 3),
           `FG%` = round(FGM / FGA, 3),
           `FT%` = round(FTM / FTA, 3)) |>
    relocate(`2P%`, .after = `2PM`) |>
    relocate(`3P%`, .after = `3PM`) |>
    relocate(`FG%`, .after = FGM) |>
    relocate(`FT%`, .after = FTM) |> 
    rename(opposition = opp_name)

#===============================================================================
# 5.) Team Metrics
#===============================================================================

team_metrics <-
    combined_stats_table |> 
    distinct(match_id, name, .keep_all = TRUE) |>
    mutate(`eFG%` = (match_two_pointers_made + 1.5 * match_three_pointers_made) / match_field_goals_attempted,
           `TS%` = match_points / (2 * (match_field_goals_attempted + 0.44 * match_free_throws_attempted)),
           `FTR` = match_free_throws_attempted / match_field_goals_attempted,
           `3PAr` = match_three_pointers_attempted / match_field_goals_attempted,
           `dRB%` = match_rebounds_defensive / (match_rebounds_defensive + opp_rebounds_offensive),
           `oRB%` = match_rebounds_offensive / (match_rebounds_offensive + opp_rebounds_defensive),
           `oRTG` = 100 * match_points / (match_field_goals_attempted + 0.44 * match_free_throws_attempted + match_turnovers),
           `dRTG` = 100 * opp_full_score / (opp_field_goals_attempted + 0.44 * opp_free_throws_attempted + opp_turnovers))

team_metrics <-
    team_metrics |> 
    group_by(name) |>
    summarise(GP = n(),
              PACE = mean(pace, na.rm = TRUE),
             `eFG%` = mean(`eFG%`),
              `TS%` = mean(`TS%`),
              `FTR` = mean(`FTR`),
              `3PAr` = mean(`3PAr`),
              `dRB%` = mean(`dRB%`),
              `oRB%` = mean(`oRB%`),
              `oRTG` = mean(`oRTG`),
              `dRTG` = mean(`dRTG`)) |>
    rename(team = name)

#===============================================================================
# 6.) Opposition Metrics
#===============================================================================

opposition_metrics <-
    combined_stats_table |> 
    distinct(match_id, name, .keep_all = TRUE) |>
    mutate(`eFG%` = (match_two_pointers_made + 1.5 * match_three_pointers_made) / match_field_goals_attempted,
           `TS%` = match_points / (2 * (match_field_goals_attempted + 0.44 * match_free_throws_attempted)),
           `FTR` = match_free_throws_attempted / match_field_goals_attempted,
           `3PAr` = match_three_pointers_attempted / match_field_goals_attempted,
           `dRB%` = match_rebounds_defensive / (match_rebounds_defensive + opp_rebounds_offensive),
           `oRB%` = match_rebounds_offensive / (match_rebounds_offensive + opp_rebounds_defensive),
           `oRTG` = 100 * match_points / (match_field_goals_attempted + 0.44 * match_free_throws_attempted + match_turnovers),
           `dRTG` = 100 * opp_full_score / (opp_field_goals_attempted + 0.44 * opp_free_throws_attempted + opp_turnovers))

opposition_metrics <-
    opposition_metrics |> 
    group_by(opp_name) |>
    summarise(GP = n(),
              PACE = mean(pace, na.rm = TRUE),
              `eFG%` = mean(`eFG%`),
              `TS%` = mean(`TS%`),
              `FTR` = mean(`FTR`),
              `3PAr` = mean(`3PAr`),
              `dRB%` = mean(`dRB%`),
              `oRB%` = mean(`oRB%`),
              `oRTG` = mean(`oRTG`),
              `dRTG` = mean(`dRTG`)) |>
    rename(opposition = opp_name)

#===============================================================================
# 7.) Player Averages Per 36
#===============================================================================

player_averages_per_36 <-
    combined_stats_table |> 
    group_by(first_name, family_name, name) |> 
    mutate(minutes_played = period_to_seconds(ms(player_minutes)) / 60) |> 
    filter(minutes_played >= 5) |> 
    summarise(GP = n(),
              MINS = sum(minutes_played),
              `2PA` = sum(player_two_pointers_attempted),
              `2PM` = sum(player_two_pointers_made),
              `3PA` = sum(player_three_pointers_attempted),
              `3PM` = sum(player_three_pointers_made),
              FGA = sum(player_field_goals_attempted),
              FGM = sum(player_field_goals_made),
              FTA = sum(player_free_throws_attempted),
              FTM = sum(player_free_throws_made),
              dREB = sum(player_rebounds_defensive),
              oREB = sum(player_rebounds_offensive),
              tREB = sum(player_rebounds_total),
              AST = sum(player_assists),
              STL = sum(player_steals),
              BLK = sum(player_blocks),
              PF = sum(player_fouls_personal),
              PFV = sum(player_fouls_received),
              TO = sum(player_turnovers),
              PTS = sum(player_points)) |> 
    mutate(`2PA` = 36 * (`2PA` / MINS),
           `2PM` = 36 * (`2PM` / MINS),
           `3PA` = 36 * (`3PA` / MINS),
           `3PM` = 36 * (`3PM` / MINS),
           FGA = 36 * (FGA / MINS),
           FGM = 36 * (FGM / MINS),
           FTA = 36 * (FTA / MINS),
           FTM = 36 * (FTM / MINS),
           dREB = 36 * (dREB / MINS),
           oREB = 36 * (oREB / MINS),
           tREB = 36 * (tREB / MINS),
           AST = 36 * (AST / MINS),
           STL = 36 * (STL / MINS),
           BLK = 36 * (BLK / MINS),
           PF = 36 * (PF / MINS),
           PFV = 36 * (PFV / MINS),
           TO = 36 * (TO / MINS),
           PTS = 36 * (PTS / MINS)) |>
    mutate(across(where(is.double), ~ round(., 2))) |>
    select(-MINS)
           
#===============================================================================
# Combine and then write out as an RDS object containing a list of dataframes
#===============================================================================

combined_data <-
    list(
        "Player Averages" = player_averages,
        "Player Averages Per 36" = player_averages_per_36,
        "Player Metrics" = player_metrics,
        "Team Averages" = team_averages,
        "Team Metrics" = team_metrics,
        "Opposition Averages" = opposition_averages,
        "Opposition Metrics" = opposition_metrics
    )

write_rds(combined_data, "Data/nbl_analytics_data.rds")
