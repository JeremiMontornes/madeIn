# madeIn

Reproducible R project calculating the French value-added content of French internal final demand by broad sector, using OECD TiVA.

The script downloads OECD TiVA 2025 data from the SDMX endpoint and computes, for each sector:

```text
French VA content = FD_VA(FRA, sector, FRA) / FD_VA(FRA, sector, W)
```

where:

- `FD_VA` is value added embodied in final demand.
- `FRA` as final demand country is French internal final demand.
- `FRA` as counterpart/source country is French value added.
- `W` as counterpart/source country is all value-added origins.

The values of `FD_VA` are in USD million in TiVA. The chart reports the French share in percent and the CSV also keeps the levels.

## Sectors

The broad sector mapping uses OECD ISIC Rev. 4 activity aggregates:

| Label | TiVA activity code |
| --- | --- |
| Agriculture | `A` |
| Energy | `D_E` |
| Manufacturing Industry | `C` |
| Construction | `F` |
| Market Services | `GTN` |
| Non-Market Services | `OTQ` |

`Energy` is the OECD aggregate for electricity, gas, water supply, sewerage, waste and remediation activities. `Market Services` is services of the business economy, sections G to N. `Non-Market Services` is public administration, defence, education, human health and social work activities.

## Run

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe' scripts\build_france_domestic_va.R
```

The R script downloads the OECD SDMX series when they are not already cached, then writes the CSV and SVG outputs. It uses base R only.

If R is blocked from network access on Windows, first populate the raw OECD SDMX cache with the helper PowerShell script, then rerun the R script:

```powershell
powershell -ExecutionPolicy Bypass -File scripts\download_tiva_cache.ps1
& 'C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe' scripts\build_france_domestic_va.R
```

Outputs:

- `data/french_va_content_in_french_internal_final_demand_by_sector.csv`
- `figures/french_va_content_in_french_internal_final_demand_by_sector.svg`
- `figures/french_va_content_in_french_internal_final_demand_by_sector.png`
