# 30by30 Maps - Shiny App

An R Shiny web application for visualizing data and progress toward "30 by 30" conservation goals. This app presents several maps that explore BII change within protected areas and areas deliverying key ecosystem services.

## Features

- Select a country to generate and view maps specific to that country
- Download individual maps
- Toggle logo and copywrite on and off

## Project Structure

- `app.R` — Main Shiny app script
- `www/` — Static files (logo files)
- `R/` — Contains map_function.R used in the app
- `pre-processing/` — Contains R scripts used to prepare data before the app is run 
- `map-data/` — contains global data layers 
- `country-data/` — contains data layers split by country

## Pre-Processing

There are two pre-processing scripts to prepare the data for use in the shiny app:
- `preprocessing_country.R` - Splits the global data layers by country using ADM0_A3. This step prepares country-specific datasets that are used by the app, significantly reducing load times when the application starts. **This pre-processing script must be re-run each time the underlying biodiversity or spatial data is updated.**
- `wdpa_download.R` - Checks for the current version of the WDPA dataset and downloads it if needed.

## Required data

- BII at (2km) for 2000 and 2020
- Country boundaries shapefile (M49_countries)
- Country boundary lines (ne_10m_admin_0_boundary_lines_land.shp)
- WDPA (as one global file, or folder with multiple files)
- CNA (CNA_terrestrial.tif) 
- Base layers:
  - sr_50m_landonly.tif
  - OB_50M.tif
  - ne_50m_ocean.shp
