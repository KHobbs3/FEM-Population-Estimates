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
out_folder <- 'intermediates/top_visited/'

# READ DATA ----
##FACILITIES ----
selected_facilities <- read.csv("intermediates/top_visited/3-manual_selection/top_facilities_to_stock_manual.csv")

# get the names of the unique stations
stations <- selected_facilities %>%
              dplyr::select(Radio.Station) %>%
              distinct() %>%
              as.list()


# Convert the data frame to an sf object, specifying the LAT and LON columns as coordinates
sf_data <- st_as_sf(selected_facilities, coords = c("LON", "LAT"), crs = 4326) # EPSG:4326 is WGS 84 (common for lat/lon)

## Reproject --------------
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


# Estimate marginal population using Leave-One-Out method ----
sf_station_master <- data.frame(matrix(ncol = length(colnames(sf_data)), nrow = 0))
colnames(sf_station_master) <- colnames(sf_data)


for (i in seq(length(stations[[1]]))){
    print(stations[[1]][i])
  
    # 0. Set-up: read station and reproject
    station_name <- stations[[1]][i]
    
    file_path <- list.files(here("../../radio/stations_gpkg/DRC/"),
            pattern = sprintf("%s\\.gpkg$", station_name), 
            recursive = TRUE, 
            full.names = TRUE)
    station <- st_read(file_path)
    station <- st_transform(station, proj)
    
    # 1. Subset points pertaining to the station
    sf_station_pts <- sf_data %>% filter(Radio.Station == station_name)
    
    # 2. Add a buffer to the points
    # N.B. We are curious to know how much each point contributes to the total so dissolving the buffer
    # (st_union) is not required
    sf_station_buffer <- sf_station_pts %>% 
                              st_buffer(5000)
    
    # 3a. Clip to station raster
    # Store output of within spatial join
    intersect_points <- st_intersection(sf_station_buffer, station)
    
    # 3b. Calculate the population coverage of all points in the layer
    intersect_points$total.population.coverage <- pop_coverage(intersect_points)
    
    
    # 4. Leave-One-Out: Loop through each point, remove it, calculate the population, then add to matrix
    for (i in seq(nrow(intersect_points))) {
        # Exclude the ith point
        sf_points_loo <- intersect_points[-i, ]
        
        # Calculate population coverage for the remaining points
        intersect_points$marginal.population.coverage[i] <- pop_coverage(sf_points_loo)
        
    }
    
    # 5. Calculation the population proportion
    intersect_points <- intersect_points %>% mutate(
      marginal.proportion = 1- marginal.population.coverage/total.population.coverage
    )
    
    
    # 6. Append data to station master dataframe
    sf_station_master <- rbind(sf_station_master, intersect_points)

    # 7. Export GPKG
    st_write(intersect_points, sprintf("output/manual_selection/manual_selection_5km_%s.gpkg", station_name), 
             append=F)

}

# 8. Export radio station summary for x-KM
write.xlsx(st_drop_geometry(sf_station_master), file = "output/manual_selection/manual_summary_5km.xlsx")

# 9. Remove facilities that require stocking but do not add to the population coverage
master_reduced <- sf_station_master %>%
  filter(!(Requires.FEM.Stocking == TRUE & marginal.proportion < 0.01))

# Export summary in GPKG and CSV formats
st_write(master_reduced, dsn = "output/manual_selection/REDUCED_manual_5km.gpkg",
         layer = "manual_reduced_5km", driver = "GPKG", delete_dsn = TRUE)
write.xlsx(master_reduced, file = "output/manual_selection/REDUCED_manual_5km.xlsx")

# 10. Summarise
master_summary <- master_reduced %>%
  group_by(Radio.Station) %>%
  reframe(Requires.FEM.Stocking_count = sum(Requires.FEM.Stocking == TRUE),
  )

for (i in seq(length(stations[[1]]))){
  
  print(stations[[1]][i])
  
  # 0. Set-up: read station and reproject
  station_name <- stations[[1]][i]
  file_path <- list.files(here("../../radio/stations_gpkg/DRC/"),
                          pattern = sprintf("%s\\.gpkg$", station_name), 
                          recursive = TRUE, 
                          full.names = TRUE)
  station <- st_read(file_path)
  station <- st_transform(station, proj)
  
  # Subset points pertaining to the station
  master_subset <- master_reduced %>% filter(Radio.Station == station_name) 
  
  # Compute population coverage
  coverage_value <- pop_coverage(master_subset)
  
  # Assign the coverage value to the corresponding row in master_summary
  master_summary[master_summary$Radio.Station == station_name, "New.Extent.Coverage"] <- coverage_value
}


# View for validation
mapview(list(sf_station_master, master_reduced),
        col.regions = list("yellow", "blue"))

# Export Summary
write.xlsx(master_summary, file = "output/manual_selection/REDUCED_summary_5km.xlsx")

# TODO: COUNT NEARBY FACILITIES ---------------
## Function ----
# Set a threshold distance (e.g., 5000 meters)
# distance_threshold <- 500

# Calculate the number of points from 'layer2' near each point in 'layer1'
# 
# #TODO: Correct the following - 
# sf_data$nearby_count <- sapply(st_geometry(sf_data), function(point) {
#   sum((point, st_geometry(sf_object_unique), dist = distance_threshold)[[1]])
# })
# 
# distance_matrix <- st_distance(sf_object)
# 
# distance_matrix <= 5000
