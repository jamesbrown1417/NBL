#!/bin/bash

# Give access to normal path vars
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd ~/Projects/NBL || exit

# Remove .json and .txt files in specific directories
rm OddsScraper/Bet365/HTML/*.txt

# Execute Python and R scripts
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Bet365/01-get_bet365_html.py
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/TAB/get-TAB-response.py
Rscript OddsScraper/Bet365/03-scrape_bet365-h2h.R
Rscript OddsScraper/Bet365/04-scrape_bet365-player.R

# Execute R script for getting all odds
Rscript OddsScraper/master_processing_script.R

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main

# Publish report using Quarto
echo "1" | quarto publish quarto-pub Reports/odds_summary.qmd
