# Get the current script's directory
$scriptPath = Split-Path -Path $MyInvocation.MyCommand.Definition -Parent

# Change to parent directory
Set-Location -Path (Split-Path -Path $scriptPath -Parent)

# Move to the "Reports" directory
Set-Location -Path ".\Reports\2023"

# Use Quarto to generate the reports
quarto render "04-player_performance.qmd"