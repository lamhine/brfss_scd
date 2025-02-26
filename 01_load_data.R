# 01_load_data.R
# Purpose: Load BRFSS data from .XPT files using the RStudio Project setup

# Load required packages
library(tidyverse)
library(haven)

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() || is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration
source("config.R")

# Define years to load
years <- 2019:2023

# Initialize an empty list to store data
brfss_data <- list()

# Load each year’s BRFSS file
for (year in years) {
  file_path <- file.path(data_dir, paste0("LLCP", year, ".XPT"))
  
  if (file.exists(file_path)) {
    df <- read_xpt(file_path)
    df$dataset <- paste0("LLCP", year)  # Track dataset source
    brfss_data[[as.character(year)]] <- df
  } else {
    message("File not found: ", file_path, ". Ensure you have downloaded the BRFSS data as instructed in the README.")
  }
}

# Combine all years into a single dataframe
brfss_raw <- bind_rows(brfss_data)

# Save the raw dataset
saveRDS(brfss_raw, file = file.path(data_dir, "BRFSS_Raw.rds"))

message("Data loading complete. Saved as BRFSS_Raw.rds")