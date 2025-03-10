# 01_load_data.R
# Purpose: Load BRFSS data from .XPT files using the RStudio Project setup

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(haven)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# UNZIP FILES INTO NEW FOLDER IN RAW DATA DIRECTORY
# ---------------------- #

# List available .zip files in the raw data directory
zip_files <- list.files(raw_data_dir, pattern = "\\.zip$", ignore.case = TRUE, full.names = TRUE)

if (length(zip_files) == 0) {
  stop("ERROR: No .zip files found in ", raw_data_dir, ". Ensure BRFSS .zip files are placed in the correct directory.")
}

# Extract each .zip file into the "unzipped" directory inside raw_data_dir
for (zip_file in zip_files) {
  unzip(zip_file, exdir = unzipped_dir, overwrite = TRUE)
  message("Extracted: ", zip_file, " → ", unzipped_dir)
}

# Refresh the list of extracted .XPT files
extracted_files <- list.files(unzipped_dir, pattern = "\\.XPT$", ignore.case = TRUE, full.names = TRUE) %>% str_trim()

if (length(extracted_files) == 0) {
  stop("ERROR: No .XPT files were found after extracting. Ensure the .zip files contain valid .XPT datasets.")
}

message("All BRFSS .zip files successfully extracted to: ", unzipped_dir)

# ---------------------- #
# LOAD FILES INTO GLOBAL ENVIRONMENT AND PROCESS
# ---------------------- #

# Load the extracted .XPT files into a list of data frames
df_list <- setNames(lapply(extracted_files, read_xpt), basename(extracted_files))
message("Loaded ", length(df_list), " datasets.")

# Standardize dataset names (remove .XPT from df_list names)
names(df_list) <- str_remove(names(df_list), "\\.XPT$")

# Ensure SCD-modules.csv exists before attempting to read it
scd_modules_path <- file.path(processed_data_dir, "SCD-modules.csv")
if (!file.exists(scd_modules_path)) {
  stop("ERROR: 'SCD-modules.csv' not found in 'data/' folder. Ensure it exists before running this script.")
}

# Load SCD-modules.csv for filtering
scd_modules <- read_csv(scd_modules_path)
message("Loaded SCD-modules.csv")

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

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save to data folder
saveRDS(filtered_dfs, file.path(processed_data_dir, "01_filtered_data.rds"))
