# Get Historic NBL Datasets and save to data folder

#==============================================================================
# Libraries
#==============================================================================

library(tidyverse)
library(nblR)
library(openxlsx)
library(googlesheets4)
library(googledrive)

#==============================================================================
# Get Data
#==============================================================================

# Team Box Scores
match_results <-
    nbl_results(wide_or_long = "long") |>
    mutate(match_time_utc = ymd_hms(match_time_utc)) |>
    arrange(desc(match_time_utc))

# Get team info to join
match_results_join <-
match_results |> 
    mutate(home_away = ifelse(is_home_competitor == 1, "home", "away")) |>
    select(match_id, home_away, name = team_name, opp_name = opp_team_name)

# Team Box Scores
team_box_scores <-
    nblR::nbl_box_team() |>
    relocate(ot_score, .after = p4_score) |>
    select(-c(short_name, opp_short_name, efficiency_custom, tot_eff_1:tot_eff_7, -full_score, -opp_full_score)) |>
    select(-c("name", "code", "opp_name")) |> 
    left_join(match_results_join, by = c("match_id", "home_away")) |>
    relocate(name, opp_name, .after = home_away) |> 
    left_join(match_results |>
        select(match_id, round_number, match_time_utc, extra_periods_used) |>
        distinct(match_id, .keep_all = TRUE)) |>
    relocate(round_number, match_time_utc, extra_periods_used, .after = season) |>
    arrange(desc(match_time_utc))

# Player Box Scores
player_box_scores <-
nbl_box_player() |>
select(match_id,
first_name,
family_name,
name = team_name,
playing_position,
starter,
shirt_number,
minutes:active,
points_fast_break:plus_minus_points)

# # Player Team Data
# team_data <-
# nblR::nbl_pbp() |> 
#     select(match_id, team_name, first_name, family_name) |> 
#     distinct(match_id, team_name, first_name, family_name, .keep_all = TRUE) |> 
#     mutate(match_id = as.character(match_id))

# Join with Player Box Scores
player_box_scores <-
    player_box_scores |> 
    mutate(match_id = as.character(match_id))

# Combined Stats Table----------------------------------------------

# Rename variables
team_box_scores <-
    team_box_scores |>
    rename(
        match_minutes = minutes,
        match_points = points,
        match_field_goals_made = field_goals_made,
        match_field_goals_attempted = field_goals_attempted,
        match_field_goals_percentage = field_goals_percentage,
        match_three_pointers_made = three_pointers_made,
        match_three_pointers_attempted = three_pointers_attempted,
        match_three_pointers_percentage = three_pointers_percentage,
        match_two_pointers_made = two_pointers_made,
        match_two_pointers_attempted = two_pointers_attempted,
        match_two_pointers_percentage = two_pointers_percentage,
        match_free_throws_made = free_throws_made,
        match_free_throws_attempted = free_throws_attempted,
        match_free_throws_percentage = free_throws_percentage,
        match_rebounds_defensive = rebounds_defensive,
        match_rebounds_offensive = rebounds_offensive,
        match_rebounds_total = rebounds_total,
        match_assists = assists,
        match_turnovers = turnovers,
        match_steals = steals,
        match_blocks = blocks,
        match_blocks_received = blocks_received,
        match_fouls_personal = fouls_personal,
        match_fouls_received = fouls_on,
        match_points_second_chance = points_second_chance,
        match_points_fast_break = points_fast_break,
        match_points_in_the_paint = points_in_the_paint
    ) |>
    filter(!is.na(p1_score) &
               !is.na(p2_score) &
               !is.na(p3_score) &
               !is.na(p4_score))

 player_box_scores <-
 player_box_scores |>
 rename(
    player_minutes = minutes,
    player_points = points,
    player_field_goals_made = field_goals_made,
    player_field_goals_attempted = field_goals_attempted,
    player_field_goals_percentage = field_goals_percentage,
    player_three_pointers_made = three_pointers_made,
    player_three_pointers_attempted = three_pointers_attempted,
    player_three_pointers_percentage = three_pointers_percentage,
    player_two_pointers_made = two_pointers_made,
    player_two_pointers_attempted = two_pointers_attempted,
    player_two_pointers_percentage = two_pointers_percentage,
    player_free_throws_made = free_throws_made,
    player_free_throws_attempted = free_throws_attempted,
    player_free_throws_percentage = free_throws_percentage,
    player_rebounds_defensive = rebounds_defensive,
    player_rebounds_offensive = rebounds_offensive,
    player_rebounds_total = rebounds_total,
    player_assists = assists,
    player_turnovers = turnovers,
    player_steals = steals,
    player_blocks = blocks,
    player_blocks_received = blocks_received,
    player_fouls_personal = fouls_personal,
    player_fouls_received = fouls_on,
    player_points_second_chance = points_second_chance,
    player_points_fast_break = points_fast_break,
    player_points_in_the_paint = points_in_the_paint
 ) |> mutate(match_id = as.character(match_id))

 # Create pace variable----------------------------------------------------------
 # Get vars
 home_team_match_data <-
     team_box_scores |>
     filter(home_away == "home") |>
     select(
         match_id,
         match_minutes,
         home_name = name,
         home_fga = match_field_goals_attempted,
         home_fgm = match_field_goals_made,
         home_fta = match_free_throws_attempted,
         home_dreb = match_rebounds_defensive,
         home_oreb = match_rebounds_offensive,
         home_to = match_turnovers
     )
 
 away_team_match_data <-
     team_box_scores |>
     filter(home_away == "away") |>
     select(
         match_id,
         match_minutes,
         away_name = name,
         away_fga = match_field_goals_attempted,
         away_fgm = match_field_goals_made,
         away_fta = match_free_throws_attempted,
         away_dreb = match_rebounds_defensive,
         away_oreb = match_rebounds_offensive,
         away_to = match_turnovers
     )
 
 # get PACE
 pace <-
     home_team_match_data |>
     left_join(away_team_match_data, by = c("match_id", "match_minutes")) |>
     mutate(match_minutes = as.integer(str_extract(match_minutes, "[0-9]+(?=:)"))) |>
     mutate(match_minutes = round(match_minutes / 5) * 5) |>
     filter(match_minutes >= 200) |>
     mutate(home_possessions = (
         home_fga + 0.4 * home_fta - 1.07 * (home_oreb / (home_oreb + away_dreb)) * (home_fga - home_fgm) + home_to
     )) |>
     mutate(away_possessions = (
         away_fga + 0.4 * away_fta - 1.07 * (away_oreb / (away_oreb + home_dreb)) * (away_fga - away_fgm) + away_to
     )) |> mutate(possessions = (home_possessions + away_possessions) / 2) |>
     mutate(pace = 200*(possessions) / match_minutes) |> 
     transmute(match_id, possessions = round(possessions, 1), pace = round(pace, 1)) |> 
     mutate(match_id = as.character(match_id))

# Combine
combined_stats_table <-
team_box_scores |>
    full_join(player_box_scores,
     by = c("match_id", "name"),
     relationship = "many-to-many") |>
    left_join(pace) |> 
    mutate(first_name = str_replace(first_name, "^Mitch$", "Mitchell")) |> 
    mutate(first_name = str_replace(first_name, "^Jordon$", "Jordan")) |> 
    mutate(first_name = str_replace(first_name, "^Dj$", "DJ")) |>
    mutate(family_name = str_replace(family_name, "^Mccarron$", "McCarron")) |>
    mutate(family_name = str_replace(family_name, "^Le'afa$", "Le'Afa")) |>
    mutate(family_name = str_replace(family_name, "^Mcdaniel$", "McDaniel")) |>
    mutate(family_name = str_replace(family_name, "^Kell Iii$", "Kell")) |>
    mutate(family_name = str_replace(family_name, "^Mcveigh$", "McVeigh")) |> 
    mutate(family_name = str_replace(family_name, "^Mcdowell$", "McDowell")) |> 
    mutate(date_scraped = ymd(Sys.Date())) |> 
    relocate(date_scraped, .before = match_id)

#==============================================================================
# Write to Data Folder
#==============================================================================

# RDS----------------------------------------------------------------
write_rds(combined_stats_table, "Data/combined_stats_table.rds")

# Excel--------------------------------------------------------------

# Create a workbook
wb <- createWorkbook()

# Add a worksheet to the workbook
addWorksheet(wb, "Combined Stats Table")

# Write the data frame to the worksheet
writeData(wb, "Combined Stats Table", combined_stats_table)

# Set auto width for columns
for(i in 1:ncol(combined_stats_table)) {
  setColWidths(wb, "Combined Stats Table", cols=i, widths="auto")
}

# Enable filters
addFilter(wb, "Combined Stats Table", col=1:ncol(combined_stats_table), rows = 1)

# Add some basic style for aesthetics
style <- createStyle(textDecoration = "bold", fgFill = "#4F81BD", halign = "center")
addStyle(wb, "Combined Stats Table", style = style, rows = 1, cols = 1:ncol(combined_stats_table), gridExpand = TRUE)

# Save workbook to Excel file
saveWorkbook(wb, "Data/combined_stats_table.xlsx", overwrite = TRUE)

