# 01_load_data.R
# Purpose: Load BRFSS data from .XPT files using the RStudio Project setup

# Load required packages
library(tidyverse)
library(haven)
library(stringr)
library(future.apply)  # For parallel processing
library(rstudioapi)    # Ensure RProj environment detection

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() || is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration (this should define `data_dir`)
source("config.R")

# Confirm data directory is correctly set
if (!dir.exists(data_dir)) {
  stop("ERROR: The data directory does not exist. Please update 'config.R' with the correct path.")
} else {
  message("Using data directory: ", data_dir)
}

# Define key directories
zip_folder <- data_dir  # Now configurable via `config.R`
unzipped_dir <- file.path(zip_folder, "unzipped")
processed_data_dir <- file.path(getwd(), "data")

# Ensure directories exist before proceeding
dir.create(unzipped_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(processed_data_dir, showWarnings = FALSE, recursive = TRUE)

# Check for .zip files before extracting
zip_files <- list.files(zip_folder, pattern = "\\.zip$", ignore.case = TRUE, full.names = TRUE)

if (length(zip_files) == 0) {
  stop("ERROR: No .zip files found in ", zip_folder, ". Ensure BRFSS .zip files are placed in the correct directory.")
}

# Extract all .XPT files from the .zip archives
walk(zip_files, function(zip_file) {
  message("Extracting: ", zip_file)
  unzip(zip_file, exdir = unzipped_dir, overwrite = TRUE)
})

# Refresh the list of extracted .XPT files
extracted_files <- list.files(unzipped_dir, pattern = "\\.XPT$", ignore.case = TRUE, full.names = TRUE) %>% str_trim()

if (length(extracted_files) == 0) {
  stop("ERROR: No .XPT files were found after extracting. Ensure the .zip files contain valid .XPT datasets.")
}

# Load the extracted .XPT files into a list of data frames
df_list <- setNames(lapply(extracted_files, read_xpt), basename(extracted_files))
message("Loaded ", length(df_list), " datasets.")

# Standardize dataset names (remove .XPT from df_list names)
names(df_list) <- str_remove(names(df_list), "\\.XPT$")

# Ensure SCD-modules.csv exists before attempting to read it
scd_modules_path <- file.path(getwd(), "data", "SCD-modules.csv")
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

# Save to data folder
saveRDS(filtered_dfs, file.path(processed_data_dir, "filtered_dfs.rds"))
message("Data processing complete. Filtered datasets saved in 'data/' folder.")