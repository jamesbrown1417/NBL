# Get Historic NBL Datasets and save to data folder

#==============================================================================
# Libraries
#==============================================================================

library(tidyverse)
library(nblR)
library(openxlsx)
library(googlesheets4)

#==============================================================================
# Get Data
#==============================================================================

# Team Box Scores
match_results <-
    nbl_results(wide_or_long = "long") |>
    mutate(match_time_utc = ymd_hms(match_time_utc)) |>
    arrange(desc(match_time_utc))

# Team Box Scores
team_box_scores <-
    nblR::nbl_box_team() |>
    relocate(ot_score, .after = p4_score) |>
    select(-c(short_name, opp_short_name, efficiency_custom, tot_eff_1:tot_eff_7, -full_score, -opp_full_score)) |>
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
playing_position,
starter,
shirt_number,
minutes:active,
points_fast_break:plus_minus_points)

# Player Team Data
team_data <-
nblR::nbl_pbp() |> 
    select(match_id, team_name, first_name, family_name) |> 
    distinct(match_id, team_name, first_name, family_name, .keep_all = TRUE)

# Join with Player Box Scores
player_box_scores <-
    player_box_scores |> 
    left_join(team_data) |> 
    relocate(team_name, .after = family_name) |> 
    rename(name = team_name)

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
    select(-match_minutes) |>
    mutate(
        possessions = 0.5 * (
            match_field_goals_attempted -
                match_rebounds_offensive +
                match_turnovers +
                0.4 * match_free_throws_attempted
        )
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
 )

# Create pace variable----------------------------------------------------------
# Get vars
home_team_match_data <-
     team_box_scores |>
     filter(home_away == "home") |>
     select(match_id, home_name = name, home_possessions = possessions)

away_team_match_data <-
     team_box_scores |>
    filter(home_away == "away") |> 
    select(match_id, home_away, away_name = name, away_possessions = possessions)

# get opp team possessions
possessions <-
    home_team_match_data |> 
    left_join(away_team_match_data, by = "match_id") |> 
    mutate(possessions = (home_possessions + away_possessions) / 2)

# Combine
combined_stats_table <-
team_box_scores |>
    full_join(player_box_scores,
     by = c("match_id", "name"),
      relationship = "many-to-many")

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

# Google Sheets-----------------------------------------------------
sheet <- gs4_find("https://docs.google.com/spreadsheets/d/1dmJK8doTFfJ3DtypqOPhsIGbQH3NBo2t0DluVU-FHXo/edit#gid=0")
sheet_write(sheet, data = combined_stats_table, sheet = "combined_stats_table")
