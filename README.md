## Data

The project uses ABS population and spatial data, SEIFA data and
Victorian GTFS public transport schedule data.

Large raw and intermediate datasets are not included in this repository
due to file-size constraints.

The processing workflow is:

Raw source data
→ `R/data-exploration-code.R`
→ cleaned analytical data
→ `R/data-visualisation-prep.R`
→ Shiny-ready datasets in `data/processed/`
→ `app/app.R`

`map_data.rds`, used by the interactive map, is also excluded due to
its file size.

Source and download instructions are provided below.

| Dataset | Source | Purpose |
| --- | --- | --- |
| Population Estimates 2001–2024 | Australian Bureau of Statistics (ABS) | Historical SA2 population growth |
| GTFS Schedule | Victorian Government | Public transport service provision |
| Census SA2 GeoPackage | Australian Bureau of Statistics (ABS) | SA2 geographic boundaries |
| SEIFA 2021 | Australian Bureau of Statistics (ABS) | Socioeconomic disadvantage |

## Reproducing the Project

1. Download the required source datasets.
2. Update the input file paths in `R/data-exploration-code.R` if required.
3. Run `R/data-exploration-code.R` to clean and integrate the source data.
4. Run `R/data-visualisation-prep.R` to generate the datasets required
   by the Shiny dashboard.
5. Run `app/app.R`.
