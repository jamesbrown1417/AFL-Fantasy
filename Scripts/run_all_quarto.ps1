# Get the current script's directory
$scriptPath = Split-Path -Path $MyInvocation.MyCommand.Definition -Parent

# Change to parent directory
Set-Location -Path (Split-Path -Path $scriptPath -Parent)

# Run the R script to generate the data in apps
Rscript ".\Apps\01-get-data.R"

# Move to the "Reports" directory
Set-Location -Path ".\Reports\2023"

# Use Quarto to generate the reports
quarto render "02-weekly-CBAs.qmd"
quarto render "04-player_performance.qmd"
quarto render "05-CBA-change-report.qmd"
quarto render "06-heatmaps.qmd"
quarto render "10-defence-vs-position.qmd"