# Script to create clusters of facilities that are within x-km of each other
# and estimate the population covered by each cluster

# Installation - run once
# install.packages(c("sf", "dbscan", "dplyr", "raster", "geosphere", "here", "data.table"))

# Load required libraries
library(sf)
library(dbscan)
library(dplyr)
library(raster)
library(geosphere)
library(here)
library("mapview")
library(data.table)


# DEFINE VARIABLES ------
# projection for epsg:102022
proj <- "+proj=aea +lat_0=0 +lon_0=25 +lat_1=20 +lat_2=-23 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"
kilometres <- c(1,2,3,4,5)


# Create function for pop coverage estimation
# Loop over each cluster, extract the raster values, and compute the population
pop_coverage <- function(i) {
  
  # Subset each cluster
  cluster <- dissolved_buffers[i, ]
  
  # Crop the raster to the cluster to speed up processing
  cropped_raster <- crop(population_raster, cluster)
  
  # Mask the raster to only include values within the cluster
  masked_raster <- mask(cropped_raster, cluster)
  
  # Calculate the total population within the cluster
  sum(values(masked_raster), na.rm = TRUE)
}


# READ DATA ----
# 1. Read in the point layer (assuming it's in a shapefile or GeoJSON)
point_layer <- st_read(here("../stockouts/output/summary/stockouts_per_facility_NAs.geojson"),
                       crs=4326)

# read in stocked facilities
stockouts_layer <- st_read(here("../stockouts/output/summary/fac_stockouts_30_cutoff_geo.geojson"),
                           crs=4326)

# reproject stocked facilties
stockouts_layer <- st_transform(point_layer, crs = proj)

# 2. Reproject the point layer to EPSG:102024 (World Cylindrical Equal Area)
point_layer <- st_transform(point_layer, crs = proj)

# export for validation
st_write(point_layer, dsn = "intermediates/1_reprojected.gpkg",
         layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)

# Read in the population raster
population_raster <- raster("../../population/cod_ppp_2020_UNadj_constrained.tif")

# Set CRS 
population_raster <- projectRaster(population_raster, crs = proj)

# ANALYSIS ----------
for (km in kilometres) {
    
    print(km)

              # DBSCAN ---------
              # 3. Perform DBSCAN clustering with minPts = 2 and distance = 1 km (1000 meters)
              # Convert to matrix of coordinates
              coords <- st_coordinates(point_layer)
              db <- dbscan(coords, eps = km*1000, minPts = 2)
              
              # Add the DBSCAN results to the point layer
              point_layer$cluster_id <- db$cluster
              
              # Fill "noise" points (those not near at least one other point) with an id
              # This step is necessary for computing convex hulls based on cluster id
              
              # Initialize start number
              start <- max(point_layer$cluster_id)+1
              
              # Determine number of single points
              n <- point_layer %>%
                        filter(cluster_id == 0) %>%
                        n_distinct()
              
              # Sequence for labels
              sequence <- seq(start, start+n) 
              
              # Start of sequence
              index = 1
              
              # Iterate through cluster and update id values
              for (i in seq_len(nrow(point_layer))) {
                
                # if the cluster_id is 0
                if (point_layer$cluster_id[i] == 0) {
                  
                    # update it with the value in the sequence 
                    point_layer$cluster_id[i] <- sequence[index]
                    
                    # Move to the next value in the sequence
                    index <- index + 1  
                  
                }
              }
              
              # Add the size of each cluster as a new column
              point_layer <- point_layer %>%
                group_by(cluster_id) %>%
                mutate(cluster_size = n())

              # Add facility names from the 'name' column to the cluster as a concatenated string
              point_layer <- point_layer %>%
                group_by(cluster_id) %>%
                mutate(facilities = paste(name, collapse = ", ")) %>%
                ungroup()
              
              # export for validation
              st_write(point_layer, dsn = sprintf("intermediates/2_dbscan_%skm.gpkg", km),
                       layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)
              
              
              # Add dissolved buffers to each cluster ----
              # 4. Compute the convex hull for each cluster
              dissolved_buffers <- point_layer %>%
                st_buffer(km*1000) %>%
                group_by(cluster_id) %>%
                summarize(geometry = st_union(geometry))
              
              #result
              mapview::mapview( list(point_layer, dissolved_buffers) )
              
              # export for validation
              st_write(dissolved_buffers, dsn = sprintf("intermediates/3_dissolved_buffers_%skm.gpkg",km),
                       layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)
              
              
              # # Convex Hull ----
              # 
              # # 4. Compute the convex hull for each cluster
              # convex_hulls <- point_layer %>%
              #   group_by(cluster_id) %>%
              #   summarize(geometry = st_combine(geometry)) %>%
              #   st_buffer(km*1000) %>%
              #   st_convex_hull() 
              # 
              # # # Add convex_hull to database # did not use!
              # # dissolved_buffers$convex_hull_pts <- point_layer %>%
              # #                                         group_by(cluster_id) %>%
              # #                                         summarize(geometry = st_combine(geometry)) %>%
              # #                                         st_buffer(km*1000) %>%
              # #                                         st_convex_hull() %>%
              # #                                         nrow()
              # 
              # 
              # #result
              # mapview::mapview( list(point_layer, dissolved_buffers, convex_hulls) )
              # 
              # 
              # # Compute the number of outer points in a convex hull and add it to the 
              # 
              # # export for validation
              # st_write(convex_hulls, dsn = sprintf("intermediates/3_chulls_%skm.gpkg",km),
              #          layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)

              
              # POPULATION ------
              # SLOW!!! Compute the population coverage of the convex hull
              # Apply population coverage function to clusters
              dissolved_buffers$population_coverage <- sapply(1:nrow(dissolved_buffers), pop_coverage)
              
              # Link back to point layer to get facility names
              population_clusters <- dissolved_buffers %>%
                                        st_join(point_layer)
              
              
              # Sanity check that spatial join matched records with same cluster_id
              population_clusters %>% 
                filter(cluster_id.x != cluster_id.y) %>%
                n_distinct()
              
              # Clean database
              colnames(population_clusters)
              
              
              # EXPORT ----------
              st_write(population_clusters, dsn = sprintf("dbscan/intermediates/4_population_coverage_%skm.gpkg", km),
                       layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)
              
              #result
              mapview::mapview( list(point_layer, population_clusters, dissolved_buffers),
                                col.regions = list("blue", "red", 'yellow'))
              
              # SUMMARIZE ---------
              # Summarize population clusters and add labels for stockout points
              summary <- population_clusters %>%
                mutate(
                  cluster_id = cluster_id.x,
                  stockout = ifelse(
                    st_within(geometry, stockouts_layer, sparse = FALSE),
                    TRUE,
                    FALSE
                  )  # Label as TRUE if the point is within the stockouts_layer
                ) %>%
                dplyr::select(cluster_id, cluster_size, population_coverage, name, stockout, geometry) %>%
                group_by(cluster_id) %>%
                summarize(
                  cluster_size = first(cluster_size),
                  population_coverage = first(population_coverage),
                  name = paste(unique(name), collapse = ", "),
                  stockout = any(stockout),  # Mark cluster as having a stockout if any point in the cluster is labeled TRUE
                  geometry = st_union(geometry)
                ) %>%
                st_as_sf()
              
              # Write the summary to a file
              st_write(summary, dsn = sprintf("output/cluster_populations/population_summary_%skm_LABELLED.gpkg", km),
                       layer = "reprojected_facilities", driver = "GPKG", delete_dsn = TRUE)
              
              # Convert to data table and export as CSV
              as.data.table(st_drop_geometry(summary)) %>%
                write.csv(sprintf("output/summary/population_summary_%skm_LABELLED.csv", km),
                          fileEncoding = "latin1")

}

