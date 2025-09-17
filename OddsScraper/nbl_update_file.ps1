# Set the current directory to your project folder
Set-Location -Path "C:\Users\james\OneDrive\Desktop\Projects\NBL"

# Remove .json and .txt files in specific directories
Remove-Item -Path "C:\Users\james\OneDrive\Desktop\Projects\NBL\OddsScraper\Bet365\HTML\*.txt"

# Execute Python and R scripts
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/NBL/OddsScraper/Bet365/01-get_bet365_html.py"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/NBL/OddsScraper/TAB/get-TAB-response.py"
& "Rscript" "OddsScraper\Bet365\03-scrape_bet365-h2h.R"
& "Rscript" "OddsScraper\Bet365\04-scrape_bet365-player.R"

# Execute R script for getting all odds
& "Rscript" "OddsScraper\master_processing_script.R"

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
$commitMessage = "automated commit and timestamp " + (Get-Date -Format "yyyy-MM-dd HH:mm:ss")
git commit -m $commitMessage

# Push the commit to the 'main' branch on 'origin'
git push origin main

# Publish report using Quarto
echo "1" | & "quarto" "publish" "quarto-pub" "Reports\odds_summary.qmd"
