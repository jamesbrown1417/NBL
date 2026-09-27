$ErrorActionPreference = "Stop"
$projectDir = "C:\Users\james\OneDrive\Desktop\Projects\NBL"
Set-Location -Path $projectDir

$directories = @(
    "Data\raw\odds\responses\tab",
    "Data\processed\odds",
    "Data\processed\stats"
)
$directories | ForEach-Object { New-Item -ItemType Directory -Force -Path $_ | Out-Null }

& "C:\Users\james\AppData\Local\Microsoft\WindowsApps\python3.12.exe" "OddsScraper\TAB\get-TAB-response.py"
if ($LASTEXITCODE -ne 0) { throw "TAB capture failed" }

& Rscript "OddsScraper\master_processing_script.R"
if ($LASTEXITCODE -ne 0) { throw "Odds processing failed" }
& Rscript "Scripts\12-export-web-stats.R"
if ($LASTEXITCODE -ne 0) { throw "Web stats export failed" }
# Model prices for the web odds export; a failure leaves the export without them.
& Rscript "Models\props\03-price-current-markets.R"
if ($LASTEXITCODE -ne 0) { Write-Warning "Model pricing failed; exporting odds without model prices" }
& Rscript "Scripts\13-export-web-odds.R"
if ($LASTEXITCODE -ne 0) { throw "Web odds export failed" }

git add -- Data/raw Data/processed Reports/odds_summary.html web/public/data
git diff --cached --quiet -- Data/raw Data/processed Reports/odds_summary.html web/public/data
if ($LASTEXITCODE -ne 0) {
    $commitMessage = "automated commit and timestamp " + (Get-Date -Format "yyyy-MM-dd HH:mm:ss")
    git commit -m $commitMessage -- Data/raw Data/processed Reports/odds_summary.html web/public/data
    if ($LASTEXITCODE -ne 0) { throw "Git commit failed" }
    git push origin main
    if ($LASTEXITCODE -ne 0) { throw "Git push failed" }
}

"1" | quarto publish quarto-pub "Reports\odds_summary.qmd"
if ($LASTEXITCODE -ne 0) { throw "Quarto publish failed" }
