library(tidyverse)
library(lubridate)

# Function to convert time to decimal
convert_time_to_decimal_hms <- function(time_obj) {
    # Convert to hms object
    time_obj <- hms(time_obj)

    # Extract hours and minutes
    hours <- hour(time_obj)
    minutes <- minute(time_obj)

    # Convert to decimal
    decimal_time <- hours + (minutes / 60)
    return(decimal_time)
}

# Function to get correlation between players
get_player_correlation <- function(player_data, seasons = NULL, name_a, name_b, metric_a, metric_b) {
    # Column names for later use
    col_name_a <- paste0(name_a, " ", metric_a)
    col_name_b <- paste0(name_b, " ", metric_b)

    # Get dataframe for player A
    df_player_a <-
        player_data %>%
        filter(PLAYER_NAME == name_a & SEASON_YEAR %in% seasons) |>
        select(match_id, PLAYER_NAME, all_of(metric_a)) |>
        rename(!!col_name_a := all_of(metric_a))

    # Get dataframe for player B
    df_player_b <-
        player_data %>%
        filter(PLAYER_NAME == name_b & SEASON_YEAR %in% seasons) |>
        select(match_id, PLAYER_NAME, all_of(metric_b)) |>
        rename(!!col_name_b := all_of(metric_b))

    # Merge the two dataframes
    df_merged <- inner_join(df_player_a, df_player_b, by = "match_id")

    # Compute correlation
    correlation <- cor(df_merged[[col_name_a]], df_merged[[col_name_b]], method = "pearson")
    cat(sprintf("The correlation between %s and %s is: %f\n", col_name_a, col_name_b, correlation))

    # Create plot
    ggplot(df_merged, aes(x = .data[[col_name_a]], y = .data[[col_name_b]])) +
        geom_point(color = "#3498db", alpha = 0.6, size = 3) +
        geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "dashed") +
        labs(
            x = col_name_a,
            y = col_name_b,
            title = "Player Performance Correlation",
            subtitle = sprintf("Correlation between %s and %s", col_name_a, col_name_b),
            caption = sprintf("Pearson's r: %.2f", correlation)
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
            plot.caption = element_text(hjust = 1, color = "grey50"),
            text = element_text(size = 12),
            axis.title = element_text(face = "bold"),
            legend.position = "none"
        ) +
        annotate(
            "text",
            x = max(df_merged[[col_name_a]]), y = min(df_merged[[col_name_b]]),
            label = sprintf("r = %.2f", correlation),
            hjust = 1, vjust = 0, size = 5, color = "red1", fontface = "italic"
        )
}

# Function to compare player performance w or w/o teammate
compare_performance <- function(player_data, seasons = NULL, main_name, teammate_name, metric) {
    # Filter the data for games with the main player
    df_player <-
        player_data %>%
        filter(PLAYER_NAME == main_name) %>%
        filter(SEASON_YEAR %in% seasons)

    # Find the game IDs where the teammate also played
    games_with_teammate <-
        player_data %>%
        filter(SEASON_YEAR %in% seasons) %>%
        filter(PLAYER_NAME == teammate_name) %>%
        pull(match_id)

    # Label each game as 'With Teammate' or 'Without Teammate'
    df_player <- df_player %>%
        mutate(Teammate = if_else(match_id %in% games_with_teammate, "With Teammate", "Without Teammate"))

    # Calculate mean and count for both conditions
    summary_stats <- df_player %>%
        group_by(Teammate) %>%
        summarise(mean_val = mean(!!sym(metric), na.rm = TRUE), n_games = n())

    # Create the violin plot
    plot <- ggplot(df_player, aes(x = Teammate, y = !!sym(metric), fill = Teammate)) +
        geom_violin(trim = FALSE, position = position_dodge(width = 0.9)) +
        geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
        labs(
            title = paste("Performance of", main_name, "with and without", teammate_name),
            x = "Condition",
            y = metric
        ) +
        scale_fill_manual(values = c("Without Teammate" = "orange1", "With Teammate" = "royalblue1")) +
        annotate("text",
            x = Inf, y = Inf,
            label = paste(
                "With Teammate: ", summary_stats$n_games[summary_stats$Teammate == "With Teammate"],
                " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "With Teammate"], 2), "\n",
                "Without Teammate: ", summary_stats$n_games[summary_stats$Teammate == "Without Teammate"],
                " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "Without Teammate"], 2)
            ),
            hjust = 1, vjust = 1
        ) +
        theme_minimal()

    return(plot)
}
