# Script to select facility catchment areas within radio bounds and compute the proportion of the radio bound population
# that each DBSCAN facility cluster covers
# Load required libraries
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
kilometres <- c(1,2,3,5)

# READ DATA ----
# Read in all possible stations
filepath = '/Users/kt/Documents/work/FEM/Family Planning/DRC/radio/stations_gpkg/DRC/'
gpkg_files <- list.files(path = filepath, pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE)

# Read all .gpkg files into a list of sf objects
sf_list <- lapply(gpkg_files, function(file) {
  sf_object <- st_read(file)
  sf_object$source_file <- basename(file)
  return(sf_object)
})

# Read in the population raster
population_raster <- raster("../../population/cod_ppp_2020_UNadj_constrained.tif")

# Set CRS 
population_raster <- projectRaster(population_raster, crs = proj)

# DEFINE FUNCTIONS ----
# Population coverage function
pop_coverage <- function(polygon) {
  
  # Crop the raster to the polygon to speed up processing
  cropped_raster <- crop(population_raster, polygon)
  
  # Mask the raster to only include values within the polygon
  masked_raster <- mask(cropped_raster, polygon)
  
  # Calculate the total population within the polygon
  sum(values(masked_raster), na.rm = TRUE)
}

# ITERATOR ----
# Read in population summary
for (km in kilometres){
  print(km)
  pop_poly <- st_read(sprintf('output/cluster_populations/population_summary_%skm.gpkg', km),
          crs=proj)
  

  table_list <- list()
  
  # Iterate through all radio stations and create a list of 
  # facilities *within*
  
  for (i in seq(length(sf_list))) {
    
    sheet_name <-  str_sub(gsub('\\.gpkg$',"", 
                                basename(gpkg_files[i])),1,31)
    
    # Verbosity
    print(paste(sprintf("%s of %s", i, length(sf_list)), 
                sheet_name))
    
    # read station
    station <- sf_list[[i]]
    
    # Reproject the station to match the facility polygon layer
    station <- st_transform(station, proj)
    
    # Calculate population coverage of station and add it to the buffer population dataframe
    pop_poly$station_population_R <- pop_coverage(station)
    
    # Using intersects because people slightly outside radio bounds likely move in bounds
    # !!! N.B. differs from python script
    # intersect_indices <- st_intersects(station, pop_poly, 2)
  
    # Store output of within spatial join
    # intersect_polygons <- pop_poly[unlist(intersect_indices), ] %>% 
                                    # distinct()
    intersect_polygons <- st_intersection(station, pop_poly)
    print(paste("Number of clusters within radio bounds:", nrow(intersect_polygons)))

    if (length(unlist(intersect_polygons)) > 0) {
      # Get station name and append as new column
      intersect_polygons$source_file <- gsub('\\.gpkg$',"", basename(gpkg_files[i]))
      intersect_polygons <- intersect_polygons %>%
                                              mutate(population_prop = population_coverage/station_population_R,
                                                     station_name = sheet_name)
                                          }
    
    # Append to table
    table_list[[sheet_name]] <- intersect_polygons

    # Export for each radio station
    st_write(intersect_polygons, dsn = sprintf("output/station_facility_clusters/%gkm/%s_%gkm_intersection.gpkg", km, sheet_name, km),
             layer = sprintf("%s", sheet_name), driver = "GPKG", delete_dsn = TRUE)
    
  }
  
  # Remove geometry for table export
  table_list_flat <- lapply(table_list, st_drop_geometry)
  
  # Export radio station summary for x-KM
  write.xlsx(table_list_flat, file = sprintf("output/station_facility_populations/radio_buffer_populations_%gkm_intersection.xlsx", km))
}

# check
mapview::mapview( list(intersect_polygons, station, pop_poly),
                  col.regions = list("blue", "red", "yellow"))
