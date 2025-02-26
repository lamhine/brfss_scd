# config.R
# Purpose: Automatically set up data storage location

# Attempt to detect the repository folder
repo_root <- tryCatch(getwd(), error = function(e) NULL)

# Default: Store data inside a 'data/' folder in the same directory as the scripts
data_dir <- file.path(repo_root, "data")

# Create 'data/' folder if it doesn't exist
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message("Created missing data directory: ", data_dir)
}

# Final check: If the directory still doesn’t exist, ask the user to provide a path
while (!dir.exists(data_dir)) {
  message("Could not find a valid data directory.")
  data_dir <- readline(prompt = "Enter the full path where your BRFSS data is stored: ")

  # Ensure the directory is valid before proceeding
  if (!dir.exists(data_dir)) {
    message("Invalid directory. Please try again.")
  } else {
    message("Data directory set to: ", data_dir)
    break
  }
}

# Confirm the directory is set correctly
message("Using data directory: ", data_dir)
