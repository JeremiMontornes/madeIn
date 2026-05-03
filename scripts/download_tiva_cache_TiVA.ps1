$ErrorActionPreference = "Stop"

$Root = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
$CacheDir = Join-Path $Root "data\raw"
New-Item -ItemType Directory -Force -Path $CacheDir | Out-Null

$BaseUrl = "https://sdmx.oecd.org/sti-public/rest/data/OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV"
$StartYear = 1995
$EndYear = 2022

$Activities = @("A", "D_E", "C", "F", "GTN", "OTQ")
$Origins = @("FRA", "W")

foreach ($activity in $Activities) {
    foreach ($origin in $Origins) {
        $key = "FD_VA.FRA.$activity.$origin.USD.A"
        $url = "$BaseUrl/$key" + "?startPeriod=$StartYear&endPeriod=$EndYear"
        $out = Join-Path $CacheDir "$key.xml"
        Write-Host "Downloading $key"
        Invoke-WebRequest -Uri $url -OutFile $out -UseBasicParsing -TimeoutSec 90
    }
}

Write-Host "Cache written to $CacheDir"
