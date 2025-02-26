# config.R
# Purpose: Define user-specific settings for file paths and directories

# Define the directory where BRFSS data files are stored
# Users should update this path to match their local setup

data_dir <- "/Users/USERNAME/Drive/BRFSS/Data"  # Example for Mac
# data_dir <- "C:/Users/USERNAME/Drive/BRFSS/Data"  # Example for Windows

# Ensure the directory exists, stop if not found
if (!dir.exists(data_dir)) {
  stop("ERROR: The data directory does not exist. Update 'config.R' with the correct path.")
}
