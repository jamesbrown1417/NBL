#==============================================================================#
# Helper functions
#==============================================================================#

c(
    "Adelaide 36ers",
    "Brisbane Bullets",
    "Cairns Taipans",
    "Illawarra Hawks",
    "Melbourne United",
    "New Zealand Breakers",
    "Perth Wildcats",
    "South East Melbourne Phoenix",
    "Sydney Kings",
    "Tasmania JackJumpers"
)

# Function to fix team names----------------------------------------------------

fix_team_names <- function(col) {
    col |>
        str_replace_all(pattern = "^(?=.*(?:36|^Ad)).*", replacement = "Adelaide 36ers") |>
        str_replace_all(pattern = "^(?=.*Bris).*", replacement = "Brisbane Bullets") |>
        str_replace_all(pattern = "^(?=.*Cairns).*", replacement = "Cairns Taipans") |>
        str_replace_all(pattern = "^(?=.*Ill).*", replacement = "Illawarra Hawks") |>
        str_replace_all(pattern = "^(?=.*(?:United|UTD|^Melb)).*", replacement = "Melbourne United") |>
        str_replace_all(pattern = "^(?=.*(?:New Zealand|NZ)).*", replacement = "New Zealand Breakers") |>
        str_replace_all(pattern = "^(?=.*Per).*", replacement = "Perth Wildcats") |>
        str_replace_all(pattern = "^(?=.*(?:Phoenix|S.E.M|^South)).*", replacement = "South East Melbourne Phoenix") |>
        str_replace_all(pattern = "^(?=.*Syd).*", replacement = "Sydney Kings") |>
        str_replace_all(pattern = "^(?=.*Tas).*", replacement = "Tasmania JackJumpers")
}