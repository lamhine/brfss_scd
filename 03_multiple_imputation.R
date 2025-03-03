# 03_multiple_imputation.R
# Purpose: Impute missing data for BRFSS SCD prevalence analysis

# Load required packages
library(tidyverse)
library(mice)
library(future)
library(rstudioapi)

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() || is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration
source("config.R")

# Load cleaned dataset
df <- readRDS(file.path(getwd(), "data", "BRFSS_Cleaned.rds"))

# Define variables to impute (exclude survey design variables)
survey_vars <- c("year", "dataset", "STATE", "STSTR", "PSU", "LLCPWT", "weight_adjusted")
impute_vars <- setdiff(names(df), survey_vars)

# Subset data for imputation
df_subset <- df %>% select(all_of(impute_vars))

# Check missing data summary
missing_summary <- df_subset %>% summarise(across(everything(), ~ mean(is.na(.)) * 100))
print(missing_summary)

# Define imputation methods based on variable types
methods <- map_chr(df_subset, ~ case_when(
  is.numeric(.) && length(unique(.)) > 10 ~ "pmm",
  is.factor(.) && nlevels(.) == 2 ~ "logreg",
  is.factor(.) && nlevels(.) > 2 ~ "polyreg",
  TRUE ~ "pmm"
))

# Generate predictor matrix
predictor_matrix <- quickpred(df_subset)

# Enable parallel processing
plan(multisession)

# Run multiple imputations (m = 5 for testing, increase to m = 20 for final)
imp <- mice(df_subset, m = 5, maxit = 10, seed = 500, method = methods, predictorMatrix = predictor_matrix)

# Extract completed datasets from the imputation object
imputed_data <- complete(imp, action = "all") 

# Add back in survey variables
imputed_data <- lapply(imputed_data, function(df_imp) {
  bind_cols(df %>% select(all_of(survey_vars)), df_imp)
})

# Save imputed datasets
saveRDS(imp, file.path(getwd(), "data", "BRFSS_Imputed.rds"))
saveRDS(imputed_data, file.path(getwd(), "data", "BRFSS_Imputed_Completed.rds"))

# Print summary of imputations
print(imp)