# madeIn

Reproducible calculation of the French value-added content of French internal final demand by broad sector, using OECD TiVA.

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
python scripts\build_france_domestic_va.py
```

If Python is blocked from network access on Windows, first populate the raw OECD SDMX cache with:

```powershell
powershell -ExecutionPolicy Bypass -File scripts\download_tiva_cache.ps1
python scripts\build_france_domestic_va.py
```

Outputs:

- `data/french_va_content_in_french_internal_final_demand_by_sector.csv`
- `figures/french_va_content_in_french_internal_final_demand_by_sector.svg`
