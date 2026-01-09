# Script to create GitHub repository and push code
# This script requires GitHub CLI (gh) to be installed and authenticated
# OR you can manually create the repository on GitHub website

param(
    [string]$RepoName = "CKM_42",
    [string]$GitHubUsername = "JivonKiang"
)

Write-Host "Creating GitHub repository: $RepoName" -ForegroundColor Green

# Check if GitHub CLI is installed
$ghInstalled = Get-Command gh -ErrorAction SilentlyContinue

if ($ghInstalled) {
    Write-Host "GitHub CLI found. Attempting to create repository..." -ForegroundColor Yellow
    try {
        gh repo create $RepoName --public --source=. --remote=origin --push
        Write-Host "Repository created and code pushed successfully!" -ForegroundColor Green
    } catch {
        Write-Host "Error creating repository via CLI: $_" -ForegroundColor Red
        Write-Host "Please create the repository manually and run the following commands:" -ForegroundColor Yellow
        Write-Host "  git remote add origin https://github.com/$GitHubUsername/$RepoName.git" -ForegroundColor Cyan
        Write-Host "  git branch -M main" -ForegroundColor Cyan
        Write-Host "  git push -u origin main" -ForegroundColor Cyan
    }
} else {
    Write-Host "GitHub CLI not found. Please create the repository manually:" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "1. Go to https://github.com/new" -ForegroundColor Cyan
    Write-Host "2. Repository name: $RepoName" -ForegroundColor Cyan
    Write-Host "3. Choose public or private" -ForegroundColor Cyan
    Write-Host "4. DO NOT initialize with README, .gitignore, or license" -ForegroundColor Cyan
    Write-Host "5. Click 'Create repository'" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Then run these commands:" -ForegroundColor Yellow
    Write-Host "  git remote add origin https://github.com/$GitHubUsername/$RepoName.git" -ForegroundColor Cyan
    Write-Host "  git branch -M main" -ForegroundColor Cyan
    Write-Host "  git push -u origin main" -ForegroundColor Cyan
}
