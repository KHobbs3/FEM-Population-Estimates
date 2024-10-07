"
This script reads in manually identified facilities and:
  1. Counts the number of replacement facilities (ie. number of points within x-metres of a selected facility).
  2. Estimates the marginal population change
"
library(sf)
library(dbscan)
library(dplyr)
library(raster)
library(geosphere)
library(here)
library(mapview)
library(data.table)
library(openxlsx)
library(stringr)


# DEFINE VARIABLES ------
# projection for epsg:102022
proj <- "+proj=aea +lat_0=0 +lon_0=25 +lat_1=20 +lat_2=-23 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"
km <- 0.5
out_folder <- 'intermediates/top_visited/4_summary/'

#TODO:
stations <- c("")

# READ DATA ----
##FACILITIES ----
#TODO: iterate through gpkg files for select radio stations ********----
stockouts_layer <- st_read(here("../stockouts/output/summary/fac_stockouts_30_cutoff_geo.geojson"),
                           crs=4326) %>%
                      dplyr::select(name, geometry) %>%
                      mutate(high_stocks = TRUE)

visits_layer <- st_read(here("../visits/output/visits_per_facility.geojson"),
                        crs=4326) %>%
                  dplyr::select(name, geometry) %>%
                  mutate(high_stocks = FALSE)

selected_facilities <- read.csv("intermediates/top_visited/3-manual_selection/top_facilities_to_stock_manual.csv")

# Convert the data frame to an sf object, specifying the LAT and LON columns as coordinates
sf_data <- st_as_sf(selected_facilities, coords = c("LON", "LAT"), crs = 4326) # EPSG:4326 is WGS 84 (common for lat/lon)


# CONCATENATION ---------------
sf_object <- rbind(stockouts_layer, visits_layer)


# DEDUPLICATION ---------------
# Identify duplicates based on geometry
sf_duplicates <- sf_object %>%
  group_by(geometry) %>%
  filter(n() > 1) %>%
  ungroup()

# Remove duplicates from original data
sf_object_unique <- sf_object %>%
  distinct(geometry, .keep_all = TRUE)

# Export the dropped geometries (e.g., as a CSV or shapefile)
# For CSV (without geometry)
write.csv(st_drop_geometry(sf_duplicates) %>% arrange(name), paste0(out_folder, "duplicates/dropped_geometries.csv"))

weird_dupes <- sf_object %>%
  filter(name %in% c("D'idjo Centre de Santé", "kc ASAD Poste de Santé", "kc Nguizani 5 Km Centre Médical", "kl Kinguendi Centre de Santé" ,
  "mg Binga Centre de Santé", "mg Bodala Centre de Santé", "mg Boso Dua Centre de Santé",
  "nk CBCA Butembo Centre Médical", "nk Luofu Centre de Santé", "nk UCG Clinique", "nk Walikale Hôpital Général de Référence", 
  "sk 5ème CELPA Centre Hospitalier", "tu Djombo Centre de Santé", "tu Losombo Poste de Santé", "tu Tofeke, Centre de Santé")
  ) 


## Visualize ---------------
mapview(list(sf_object, weird_dupes),
        col.regions = list("yellow", "blue"))

## Export duplicates for validation ------------
st_write(weird_dupes, dsn = sprintf("%sduplicates/validate_duplicates.gpkg",out_folder),
         layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)
st_write(sf_object, dsn = sprintf("%sduplicates/facilities_concatenates.gpkg",out_folder),
         layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)

## Reproject --------------
sf_object <- st_transform(sf_object, crs = proj)
sf_data <- st_transform(sf_data, crs = proj)


# MARGINAL POPULATION --------------
## Read in the population raster -----
population_raster <- raster("../../population/cod_ppp_2020_UNadj_constrained.tif")

# Set CRS 
population_raster <- projectRaster(population_raster, crs = proj)

## Population coverage function ----
pop_coverage <- function(polygon) {
  
  # Crop the raster to the polygon to speed up processing
  cropped_raster <- crop(population_raster, polygon)
  
  # Mask the raster to only include values within the polygon
  masked_raster <- mask(cropped_raster, polygon)
  
  # Calculate the total population within the polygon
  sum(values(masked_raster), na.rm = TRUE)
}

# TODO:
# estimate marginal population using Leave-One-Out method.




# COUNT NEARBY FACILITIES ---------------
## Function ----
# Set a threshold distance (e.g., 5000 meters)
distance_threshold <- 500

# Calculate the number of points from 'layer2' near each point in 'layer1'

#TODO: Correct the following - 
sf_data$nearby_count <- sapply(st_geometry(sf_data), function(point) {
  sum((point, st_geometry(sf_object_unique), dist = distance_threshold)[[1]])
})
