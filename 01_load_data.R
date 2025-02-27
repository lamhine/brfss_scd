# 01_load_data.R
# Purpose: Load BRFSS data from .XPT files using the RStudio Project setup

# Load required packages
library(tidyverse)
library(haven)
library(stringr)
library(future.apply)  # For parallel processing

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() || is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration (this should define `data_dir`)
source("config.R")
message("Using data directory: ", data_dir)

# Define key directories
zip_folder <- data_dir  # Now configurable via `config.R`
unzipped_dir <- file.path(zip_folder, "unzipped")
processed_data_dir <- file.path(getwd(), "data")

# Ensure directories exist
dir.create(unzipped_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(processed_data_dir, showWarnings = FALSE, recursive = TRUE)

# List all .zip files in the folder
zip_files <- list.files(zip_folder, pattern = "\\.zip$", ignore.case = TRUE, full.names = TRUE)

# Extract all .XPT files from the .zip archives
walk(zip_files, function(zip_file) {
  message("Extracting: ", zip_file)
  unzip(zip_file, exdir = unzipped_dir, overwrite = TRUE)
})

# Refresh the list of extracted .XPT files
extracted_files <- list.files(unzipped_dir, pattern = "\\.XPT$", ignore.case = TRUE, full.names = TRUE) %>% str_trim()

# Load the extracted .XPT files into a list of data frames
df_list <- setNames(lapply(extracted_files, read_xpt), basename(extracted_files))
message("Loaded ", length(df_list), " datasets.")

# Standardize dataset names (remove .XPT from df_list names)
names(df_list) <- str_remove(names(df_list), "\\.XPT$")

# Load SCD-modules.csv for filtering
scd_modules <- read_csv("SCD-modules.csv")

# Filter for only the relevant datasets
filter_dfs <- function(df_list, scd_modules) {
  # Create an empty list to store the filtered dataframes
  filtered_list <- list()
  
  # Loop through each unique dataset in scd_modules
  for (dataset_name in unique(scd_modules$dataset)) {
    # Check if the dataset_name exists in df_list
    if (dataset_name %in% names(df_list)) {
      # Get the corresponding dataframe
      df <- df_list[[dataset_name]]
      
      # Filter scd_modules for the current dataset
      dataset_fips <- scd_modules %>%
        filter(dataset == dataset_name) %>%
        pull(fips)
      
      # Filter the dataframe by `_STATE` column for matching FIPS codes
      filtered_list[[dataset_name]] <- df %>%
        filter(`_STATE` %in% dataset_fips)
    }
  }
  
  # Return the list of filtered dataframes
  return(filtered_list)
}

# Apply the filtering function
filtered_dfs <- filter_dfs(df_list, scd_modules)

# Save to data folder
saveRDS(filtered_dfs, file.path(processed_data_dir, "filtered_dfs.rds"))
message("Data processing complete. Filtered datasets saved in 'data/' folder.")
