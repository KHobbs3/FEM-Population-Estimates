# Script to summarise for each radio estimate
# * what the total population proportion would be if all clusters were included (ie. if all facilities were stocked)
# * how many clusters are there (ie. how many facility groups would need to be stocked)?
# * how many facilities are there (ie. how many facilities, at most, would need to be stocked)?


# Load necessary libraries
library(readxl)
library(dplyr)
library(purrr)
library(openxlsx)

# Set-up -------
# Define variables
kilometres <- c(1,2,3,5)

# Functions ----
summarise_df <- function(x) {

  if (base::nrow(x) > 0){
    
    x %>%
      group_by(source_file, cluster_id) %>%
      reframe(
        top_cluster_population_reach = max(population_coverage),
        station_population_R = first(station_population_R),
        total_clusters = n(),
        top_cluster_count_facility = sum(cluster_size[population_coverage == max(population_coverage)]),
        cluster_population_prop = population_coverage / station_population_R,
      ) %>%
      distinct()
  } else {
    
    # Return an empty dataframe with the expected columns
    tibble(
      source_file = character(),
      population_reach = numeric(),
      cluster_population_prop = numeric(),
      count_clusters = integer(),
      count_facility = numeric()
    )
  }

}


# Wrangle data --------
wb <- createWorkbook()

for (km in kilometres){
  
  # Read data --------
  # Define the Excel file path
  excel_file <- sprintf("output/station_facility_populations/radio_buffer_populations_%skm_intersection.xlsx", km)
  
  # Read all sheet names
  sheet_names <- excel_sheets(excel_file)
  
  # Format data ---------
  # Loop through each sheet, perform operations, and store results in a list
  df_list <- list()
  
  for (sheet in sheet_names) {
    # Read the current sheet as a dataframe
    df <- read_excel(excel_file, sheet = sheet)
    
    # Assign the dataframe to the list, using the sheet name as the list element name
    df_list[[sheet]] <- df
  }

    # Map-apply summary function and assign result 
    summary_df <- map_dfr(df_list, summarise_df, .id = sprintf("%s", km)) %>%
      dplyr::select(-1)
    
    addWorksheet(wb, sheetName = sprintf('%skm', km))
    
    # Write the summary_df to the new sheet
    writeData(wb, sheet = sprintf('%skm', km), summary_df)
    
    # Export population summary for x-KM
    today <- Sys.Date()
    
}

# Export
saveWorkbook(wb,
             file = sprintf("output/summary/population_coverage_%s.xlsx",
                            today),
             overwrite = T
             )
