
# Population coverage estimates for facility catchment areas (all facilities)

**Status**: `Active`

**Last updated**: 07 October, 2024

## Data

- DHIS2 data extracted in May 2024 for all facilities reporting
  contraceptive method stock outs from October - December 2023.
- Radio station rasters generated by CloudRF and coverted to a dissolved
  polygon GeoPackage (done in QGIS)

## Scripts

- `dbscan.R`: Script to create clusters of facilities that are within
  x-km of each other and estimate the population covered by each
  cluster.
- `radio_populations.R`: Script to select facility catchment areas
  within radio bounds and compute the proportion of the radio bound
  population \# that each DBSCAN facility cluster covers.
- `population_summary.R`: Script to summarise for each radio estimate.
  - what the total population proportion would be if all clusters were
    included (ie. if all facilities were stocked).
  - how many clusters are there (ie. how many facility groups would need
    to be stocked)?
  - how many facilities are there (ie. how many facilities, at most,
    would need to be stocked)?
- `stock_all_facilities.R`: Essentially the radio_populations.R script
  but for dissolved buffers of all facilities.
- `analyse_selected_facilities.R`: Estimates the number of replacement
  facilities and marginal population coverage of each point that is not
  sufficiently stocked.

## Roadmap

General improvements: 1. Improve linting 2. Improve generalisability of
scripts 3. Add ability to create directories if they do not exist
