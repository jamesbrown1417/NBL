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
        # South East Melbourne Phoenix (handle many vendor variants)
        str_replace_all(pattern = "^(?=.*(?:Phoenix|S\\.?.?E\\.?.?\\s*Melb(?:ourne)?|\\bSEM\\b|South\\s*East)).*", replacement = "South East Melbourne Phoenix") |>
        str_replace_all(pattern = "^(?=.*Syd).*", replacement = "Sydney Kings") |>
        str_replace_all(pattern = "^(?=.*Tas).*", replacement = "Tasmania JackJumpers")
}

# Function to fix/normalise player names---------------------------------------

# Applies a consistent set of replacements across scrapers so edits live
# in one place. Safe to use on full names, initial+surname (e.g. "M Dellavedova"),
# or strings that contain player names within longer phrases.
fix_player_names <- function(col) {
    col |>
        # Common abbrev./spelling normalisations
        str_replace_all("^Mitch\\b", "Mitchell") |>
        str_replace_all("\\bMcveigh\\b", "McVeigh") |>
        str_replace_all("\\bMatthew Mooney\\b", "Matt Mooney") |>
        str_replace_all("\\bWilliam McDowell White\\b", "Will McDowell-White") |>
        str_replace_all("\\bMatthew William Dellavedova\\b", "Matthew Dellavedova") |>
        str_replace_all("\\bLe Afa\\b", "Le'Afa") |>
        str_replace_all("Jo Lual-Acuil Jr\\.", "Jo Lual-Acuil Jr") |>
        str_replace_all("^Jo Lual-Acuil$", "Jo Lual-Acuil Jr") |>
        str_replace_all("\\bByrce\\b", "Bryce") |>
        str_replace_all("\\bJordon\\b", "Jordan") |>
        str_replace_all("\\bGary Brown$", "Gary Browne") |>
        str_replace_all("\\bDelaney\\b", "Delany") |>
        str_replace_all("\\bLee Jr\\.", "Lee") |>
        # Initials/short-form variants (TAB style)
        str_replace_all("\\bTeRangi\\b", "Te Rangi") |>
        str_replace_all("\\bP J-Crtwght\\b", "P Jackson-Cartwright") |>
        str_replace_all("\\bW McD-White\\b", "W McDowell-White") |>
        str_replace_all("\\bS Wardnburg\\b", "S Waardenburg") |>
        str_replace_all("\\bJ Lual-Acuil\\b", "J Lual-Acuil Jr")
}

# Function to add/standardise TAB-style player initials-------------------------

# TAB often supplies last names only or unusual substrings that need an initial
# for joining to the canonical player list. Apply this before splitting to
# first/last name in the TAB pipeline.
fix_player_initials <- function(col) {
    col |>
        # Add missing first initial for common cases
        str_replace_all("^Dellavedova\\b", "M Dellavedova") |>
        # Normalise special surname punctuation
        str_replace_all("Le Afa", "Le'Afa") |>
        # Ensure initial for Lual-Acuil variants
        str_replace_all("Lual-Acuil", "J Lual-Acuil")
}
