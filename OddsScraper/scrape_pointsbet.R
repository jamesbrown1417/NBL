# PointsBet NBL odds. Keep the existing output and SGM key contracts.
library(tidyverse)
library(httr2)
source("Scripts/04-helper-functions.R")

pointsbet_get_json <- function(url) {
    request(url) |> req_timeout(30) |> req_perform() |> resp_body_json()
}

pointsbet_empty_rows <- function() {
    tibble(match = character(), start_time = character(), home_team = character(),
           away_team = character(), event = character(), market = character(),
           outcome = character(), outcome_type = character(), price = numeric(),
           EventKey = character(), MarketKey = character(), OutcomeKey = character())
}

parse_pointsbet_event <- function(event) {
    required <- c("key", "homeTeam", "awayTeam", "startsAt", "fixedOddsMarkets")
    if (!is.list(event) || !all(required %in% names(event)) ||
        !is.list(event$fixedOddsMarkets) ||
        any(vapply(event[c("key", "homeTeam", "awayTeam", "startsAt")],
                   function(x) length(x) != 1L || is.na(x), logical(1)))) {
        stop("PointsBet: incomplete event response.")
    }
    rows <- pointsbet_empty_rows()
    if (isTRUE(event$isLive)) return(rows)
    home <- fix_team_names(event$homeTeam)
    away <- fix_team_names(event$awayTeam)
    for (market_record in event$fixedOddsMarkets) {
        if (!isTRUE(market_record$isOpenForBetting)) next
        # These exports are full-game markets only.
        if (!is.null(market_record$period) && market_record$period != "FT") next
        for (selection in market_record$outcomes) {
            if (!isTRUE(selection$isOpenForBetting) || isTRUE(selection$isHidden) ||
                is.null(selection$price) || !is.finite(selection$price) || selection$price <= 1) next
            if (is.null(market_record$key) || is.null(selection$key) || is.null(market_record$name) || is.null(selection$name)) {
                stop("PointsBet: open selection is missing its name or SGM keys.")
            }
            rows <- bind_rows(rows, tibble(
                match = paste(home, "v", away), start_time = event$startsAt,
                home_team = home, away_team = away,
                EventKey = as.character(event$key),
                event = market_record$eventName %||% NA_character_, market = market_record$name,
                outcome = fix_player_names(selection$name),
                outcome_type = selection$outcomeType %||% NA_character_,
                price = as.numeric(selection$price),
                MarketKey = as.character(market_record$key), OutcomeKey = as.character(selection$key)))
        }
    }
    rows
}

validate_pointsbet_roster <- function(data) {
    invalid <- is.na(data$player_team) |
        !(data$player_team == data$home_team | data$player_team == data$away_team)
    if (any(invalid)) {
        warning("PointsBet roster names unresolved: ", paste(unique(data$player_name[invalid]), collapse = ", "), call. = FALSE)
        data$player_team[invalid] <- NA_character_
        data$opposition_team[invalid] <- NA_character_
    }
    data
}

pointsbet_h2h_main <- function(fetch = pointsbet_get_json, output_dir = data_paths$raw_odds,
                               roster = read_csv(data_file("raw_stats", "supercoach-data.csv"), show_col_types = FALSE)) {
    player_names_teams <- roster |>
        transmute(player_full_name = fix_player_names(paste(player_first_name, player_last_name)), player_team) |>
        distinct()
    if (anyDuplicated(player_names_teams$player_full_name)) {
        stop("PointsBet: ambiguous player names in roster.")
    }
    url <- "https://api.au.pointsbet.com/api/v2/competitions/7172/events/featured?includeLive=false"
    discovery <- fetch(url)
    if (!is.list(discovery) || !("events" %in% names(discovery)) || !is.list(discovery$events)) {
        stop("PointsBet: expected a competition events list.")
    }
    if (!is.null(discovery$nextPage) && !identical(discovery$nextPage, "")) {
        stop("PointsBet: competition response is paginated; refusing an incomplete refresh.")
    }
    events <- keep(discovery$events, ~ !isTRUE(.x$isLive))
    keys <- unique(map_chr(events, function(event) {
        if (is.null(event$key) || length(event$key) != 1) stop("PointsBet: event key is missing.")
        as.character(event$key)
    }))
    # Fetch events even when featured markets are empty; props may still be available.
    pointsbet_data_player_props <- pointsbet_empty_rows()
    for (key in keys) {
        event <- fetch(paste0("https://api.au.pointsbet.com/api/mes/v3/events/", key))
        if (!identical(as.character(event$key), key)) stop("PointsBet: event key mismatch.")
        pointsbet_data_player_props <- bind_rows(pointsbet_data_player_props, parse_pointsbet_event(event))
    }
    h2h <- pointsbet_data_player_props |> filter(event == "Head to Head") |>
        mutate(outcome = fix_team_names(outcome))
    home <- h2h |> filter(outcome == home_team) |>
        select(EventKey, match, start_time, home_team, away_team, home_win = price)
    away <- h2h |> filter(outcome == away_team) |> select(EventKey, away_win = price)
    pointsbet_h2h <- inner_join(home, away, by = "EventKey", relationship = "one-to-one") |>
        transmute(match, start_time, market_name = "Head To Head", home_team, home_win,
                  away_team, away_win, margin = round(1 / home_win + 1 / away_win, 3), agency = "Pointsbet")

#===============================================================================
# Player Points
#===============================================================================

# Player points alternative totals----------------------------------------------

# Filter list to player points
pointsbet_player_points_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "To Get [0-9]{1,2}\\+ Points")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(outcome = fix_player_names(outcome)) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name = outcome,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Player points over / under----------------------------------------------------

# Filter list to player points over under
pointsbet_player_points_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Points Over/Under")) |>
    mutate(outcome = fix_player_names(outcome))

# Get Overs
pointsbet_player_points_over <-
    pointsbet_player_points_over_under |> 
    filter(outcome_type == "Over") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(player_name = fix_player_names(player_name)) |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )
    
# Get Unders
pointsbet_player_points_under <-
    pointsbet_player_points_over_under |> 
    filter(outcome_type == "Under") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(player_name = fix_player_names(player_name)) |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey_unders = OutcomeKey
    )

# Combine overs and unders
pointsbet_player_points_over_under <-
    pointsbet_player_points_over |>
    full_join(pointsbet_player_points_under, by = c("match", "home_team", "away_team", "market_name", "player_name", "player_team", "opposition_team", "line", "agency", "EventKey", "MarketKey")) |>
    select(
        match,
        home_team,
        away_team,
        market_name,
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        under_price,
        agency,
        contains("Key")
    )

#===============================================================================
# Player Assists
#===============================================================================

# Player assists alternative totals----------------------------------------------

# Filter list to player assists
pointsbet_player_assists_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "To Get [0-9]{1,2}\\+ Assists")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(outcome = fix_player_names(outcome)) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name = outcome,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Player assists over / under----------------------------------------------------

# Filter list to player assists over under
pointsbet_player_assists_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Assists Over/Under")) |>
    mutate(outcome = fix_player_names(outcome))

# Get Overs
pointsbet_player_assists_over <-
    pointsbet_player_assists_over_under |> 
    filter(outcome_type == "Over") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(player_name = fix_player_names(player_name)) |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Get Unders
pointsbet_player_assists_under <-
    pointsbet_player_assists_over_under |> 
    filter(outcome_type == "Under") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(player_name = fix_player_names(player_name)) |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey_unders = OutcomeKey
    )

# Combine overs and unders
pointsbet_player_assists_over_under <-
    pointsbet_player_assists_over |>
    full_join(pointsbet_player_assists_under, by = c("match", "home_team", "away_team", "market_name", "player_name", "player_team", "opposition_team", "line", "agency", "EventKey", "MarketKey")) |>
    select(
        match,
        home_team,
        away_team,
        market_name,
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        under_price,
        agency,
        contains("Key")
    )


#===============================================================================
# Player Rebounds
#===============================================================================

# Player rebounds alternative totals----------------------------------------------

# Filter list to player rebounds
pointsbet_player_rebounds_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "To Get [0-9]{1,2}\\+ Rebounds")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(outcome = fix_player_names(outcome)) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name = outcome,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Player rebounds over / under----------------------------------------------------

# Filter list to player rebounds over under
pointsbet_player_rebounds_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Rebounds Over/Under")) |>
    mutate(outcome = fix_player_names(outcome))

# Get Overs
pointsbet_player_rebounds_over <-
    pointsbet_player_rebounds_over_under |> 
    filter(outcome_type == "Over") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(player_name = fix_player_names(player_name)) |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Get Unders
pointsbet_player_rebounds_under <-
    pointsbet_player_rebounds_over_under |> 
    filter(outcome_type == "Under") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(player_name = fix_player_names(player_name)) |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey_unders = OutcomeKey
    )

# Combine overs and unders
pointsbet_player_rebounds_over_under <-
    pointsbet_player_rebounds_over |>
    full_join(pointsbet_player_rebounds_under, by = c("match", "home_team", "away_team", "market_name", "player_name", "player_team", "opposition_team", "line", "agency", "EventKey", "MarketKey")) |>
    select(
        match,
        home_team,
        away_team,
        market_name,
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        under_price,
        agency,
        contains("Key")
    )

#===============================================================================
# Player Threes
#===============================================================================

# Player threes alternative totals----------------------------------------------

# Filter list to player threes (matches both "To Get X+ Threes" and "X+ Made Threes" formats)
pointsbet_player_threes_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "(To (Get|Make|Record) [0-9]{1,2}\\+ (Made )?Threes)|([0-9]{1,2}\\+ Made Threes)")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(outcome = fix_player_names(outcome)) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Threes",
        player_name = outcome,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Player threes over / under----------------------------------------------------

# Filter list to player threes over under (matches multiple formats)
pointsbet_player_threes_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "(Player )?(Threes|Made Threes|Three Pointers Made) Over/Under")) |>
    mutate(outcome = fix_player_names(outcome))

# Get Overs
pointsbet_player_threes_over <-
    pointsbet_player_threes_over_under |> 
    filter(outcome_type == "Over") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(player_name = fix_player_names(player_name)) |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Threes",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Get Unders
pointsbet_player_threes_under <-
    pointsbet_player_threes_over_under |> 
    filter(outcome_type == "Under") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(player_name = fix_player_names(player_name)) |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Threes",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey_unders = OutcomeKey
    )

# Combine overs and unders
pointsbet_player_threes_over_under <-
    pointsbet_player_threes_over |>
    full_join(pointsbet_player_threes_under, by = c("match", "home_team", "away_team", "market_name", "player_name", "player_team", "opposition_team", "line", "agency", "EventKey", "MarketKey")) |>
    select(
        match,
        home_team,
        away_team,
        market_name,
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        under_price,
        agency,
        contains("Key")
    )

# Build and validate all outputs before writing any files.
outputs <- list(pointsbet_h2h = pointsbet_h2h,
                pointsbet_player_points = bind_rows(pointsbet_player_points_lines, pointsbet_player_points_over_under),
                pointsbet_player_rebounds = bind_rows(pointsbet_player_rebounds_lines, pointsbet_player_rebounds_over_under),
                pointsbet_player_assists = bind_rows(pointsbet_player_assists_lines, pointsbet_player_assists_over_under),
                pointsbet_player_threes = bind_rows(pointsbet_player_threes_lines, pointsbet_player_threes_over_under))
for (name in names(outputs)[-1]) {
    outputs[[name]] <- validate_pointsbet_roster(outputs[[name]]) |>
        select(match, home_team, away_team, market_name, player_name, player_team,
               line, over_price, under_price, agency, opposition_team,
               EventKey, MarketKey, OutcomeKey, OutcomeKey_unders)
    if (anyNA(outputs[[name]]$line)) stop("PointsBet: could not parse a player line in ", name)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
iwalk(outputs, ~ write_csv(.x, file.path(output_dir, paste0(.y, ".csv"))))
message("PointsBet: refreshed ", nrow(pointsbet_h2h), " head-to-head matches and ",
        sum(map_int(outputs[-1], nrow)), " player-prop rows.")
if (!sum(map_int(outputs[-1], nrow))) message("PointsBet: no supported player-prop markets are currently offered.")
invisible(outputs)
}

if (!isTRUE(getOption("nbl.pointsbet.skip_run", FALSE))) pointsbet_h2h_main()
