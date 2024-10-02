# Script to estimate the population reached if *all* facilities within a radio bounds were stocked
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
kilometres <- c(5,3,2,1)

# READ DATA ----
##FACILITIES ----
# 1. Read in the point layer (assuming it's in a shapefile or GeoJSON)
point_layer <- st_read(here("../stockouts/output/summary/fac_stockouts_30_cutoff_geo.geojson"),
                       crs=4326)

# 2. Reproject the point layer to EPSG:102024 (World Cylindrical Equal Area)
point_layer <- st_transform(point_layer, crs = proj)


## RADIO STATIONS ----
# 3. Read in all possible stations
filepath = '/Users/kt/Documents/work/FEM/Family Planning/DRC/radio/stations_gpkg/DRC/'
gpkg_files <- list.files(path = filepath, pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE)

# 4. Read all .gpkg files into a list of sf objects
sf_list <- lapply(gpkg_files, function(file) {
  sf_object <- st_read(file)
  sf_object$source_file <- basename(file)
  return(sf_object)
})


## POPULATION ----
# Read in the population raster
population_raster <- raster("../../population/cod_ppp_2020_UNadj_constrained.tif")

# Set CRS 
population_raster <- projectRaster(population_raster, crs = proj)

# DEFINE FUNCTIONS ----
## Population coverage function ----
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
  
  # 3. Add a buffer to *ALL* facilities (this results in chaining)
  dissolved_buffers_full <- point_layer %>%
    st_buffer(km*1000) %>%
    summarize(geometry = st_union(geometry))
  
  # Create empty list to populate with output
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
    
    # Using intersects because people slightly outside radio bounds likely move in bounds
    # !!! N.B. differs from python script
    # intersect_indices <- st_intersects(station, pop_poly, 2)
    
    # Store output of within spatial join
    intersect_points <- st_intersection(dissolved_buffers_full, station) %>% st_cast("POLYGON")
    print(paste("Number of facilities within radio bounds:", length(intersect_points)))
    
    
    # CALC: POPULATION ------
    if (nrow(intersect_points) >0 ){
        # Calculate population coverage of station and add it to the buffer population dataframe
        intersect_points$station_population_R <- pop_coverage(station)
        
        # Calculate population coverage of polygon and add it to the buffer population dataframe
        intersect_points$all_facilities_population <- pop_coverage(intersect_points)
        intersect_points <- intersect_points %>%
                                  group_by(station_population_R, all_facilities_population) %>%
                                  summarise(geometry = st_union(geometry))
        
        # export for validation
        # st_write(intersect_points, dsn = sprintf("intermediates/all_facilities_stocked/4_dissolved_buffers_all_points_%s_%skm.gpkg",sheet_name, km),
        #          layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)
        st_write(intersect_points, dsn = sprintf("intermediates/highly_stocked/intersect_stocked_30_%s_%skm.gpkg",sheet_name, km),
                 layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)

  
      # Get station name and append as new column
      intersect_points$source_file <- gsub('\\.gpkg$',"", basename(gpkg_files[i]))
      intersect_points$population_prop = intersect_points$all_facilities_population/intersect_points$station_population_R
      intersect_points$station_name = sheet_name
    }
    
    # Append to table
    table_list[[sheet_name]] <- intersect_points
    
    
    # # Export for each radio station
    # st_write(intersect_points, dsn = sprintf("output/station_facility_clusters/%gkm/%s_all_points_%gkm.gpkg", km, sheet_name, km),
    #          layer = sprintf("%s", sheet_name), driver = "GPKG", delete_dsn = TRUE)
    
  }
  
  # Remove geometry for table export
  table_list_flat <- lapply(table_list, function(x) {
    if ("sf" %in% class(x)) {
      return(st_drop_geometry(x))
    } else {
      return(x)
    }
  })
  
  # create one sheet in table
  table <- do.call(rbind, table_list_flat)
  
  # Export radio station summary for x-KM
  write.xlsx(table, file = sprintf("output/station_facility_populations/radio_buffer_populations_stocked_30_%gkm.xlsx", km))
}

# check
mapview::mapview( list(intersect_points, station),
                  col.regions = list("blue", "red"))
