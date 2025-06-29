# 02_clean_data.R
# Purpose: Clean and preprocess BRFSS data for SCD prevalence analysis

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD AND PROCESS FILTERED DATASET
# ---------------------- #

# Load the single filtered dataset
filtered_dfs <- readRDS(file.path(processed_data_dir, "01_filtered_data.rds"))

# Convert list of dataframes into a single dataframe, preserving dataset name
df <- bind_rows(filtered_dfs, .id = "dataset")  

# Extract year from dataset name
df <- df %>%
  mutate(
    year = case_when(
      str_detect(dataset, "V\\d$") ~ as.numeric(paste0("20", str_extract(dataset, "\\d{2}"))), 
      str_detect(dataset, "\\d{4}$") ~ as.numeric(str_extract(dataset, "\\d{4}$")), 
      TRUE ~ NA_real_  # Explicitly handle unexpected cases
    )
  )

# Select only variables needed (extra vars are for imputation)
df <- df %>% 
  select(year, dataset, `_STATE`, `_STSTR`, `_LLCPWT`, `_LCPWTV1`, `_LCPWTV2`,
         `_LCPWTV3`, `_PSU`, `_AGEG5YR`, SEXVAR, `_RACE`, `_RACE1`, EDUCA, 
         INCOME2, INCOME3, MENTHLTH, ADDEPEV3, `_RFDRHV7`, `_RFDRHV8`, 
         `_SMOKER3`, EXERANY2, CVDCRHD4, CVDSTRK3, DIABETE4, `_RFBMI5`, BPHIGH4, 
         BPHIGH6, TOLDHI3, TOLDHI2, CIMEMLOS, CIMEMLO1)

# Now coalesce variables that have different names across years
df <- df %>%
  mutate(
    LLCPWT = coalesce(`_LLCPWT`, `_LCPWTV1`, `_LCPWTV2`, `_LCPWTV3`),
    RACE = coalesce(`_RACE`, `_RACE1`),
    INCOME = coalesce(INCOME2, INCOME3),
    HVYDRNK = coalesce(`_RFDRHV7`, `_RFDRHV8`),
    BPHIGH = coalesce(BPHIGH4, BPHIGH6),
    TOLDHI = coalesce(TOLDHI3, TOLDHI2),
    MEMLOSS = coalesce(CIMEMLOS, CIMEMLO1)
  ) %>%
  select(-c(`_LLCPWT`, `_LCPWTV1`, `_LCPWTV2`, `_LCPWTV3`, `_RACE`, `_RACE1`, 
            INCOME2, INCOME3, `_RFDRHV7`, `_RFDRHV8`, BPHIGH4, BPHIGH6, TOLDHI3, 
            TOLDHI2, CIMEMLOS, CIMEMLO1))

# Rename problematic variables
df <- df %>% 
  rename(
    STATE = `_STATE`,
    STSTR = `_STSTR`,
    PSU = `_PSU`,
    AGEG5YR = `_AGEG5YR`,
    SMOKER3 = `_SMOKER3`,
    OVWOB = `_RFBMI5`
  )


# Define correct labels for RACE variable in alphabetical order
race_labels <- c(
  "1" = "White",
  "2" = "Black",
  "3" = "AIAN",
  "4" = "Asian",
  "5" = "NHPI",
  "6" = "Other race",
  "7" = "Multiracial",
  "8" = "Hispanic", 
  "Unknown" = "Unknown"
)

# recode problematic variables
df <- df %>%
  mutate(
    AGEG5YR = na_if(AGEG5YR, 14) %>% as.factor(),
    EDUCA = na_if(EDUCA, 9) %>% as.factor(),
    MENTHLTH = case_when(
      MENTHLTH == 88 ~ 0,  # None
      MENTHLTH %in% c(77, 99) ~ NA_real_,  # Don't know/Refused
      TRUE ~ MENTHLTH
    ),
    # Convert binary yes/no variables to factors while handling missing values
    across(c(SEXVAR, ADDEPEV3, EXERANY2, CVDCRHD4, CVDSTRK3, DIABETE4, OVWOB, 
             HVYDRNK, BPHIGH, TOLDHI, MEMLOSS),
           ~ case_when(
             . %in% c(7, 9) ~ NA_real_,  # Convert 7 and 9 to NA
             TRUE ~ .  # Keep valid values
           ) %>% as.factor()),
    SMOKER3 = case_when(
      SMOKER3 == 9 ~ NA_real_,
      TRUE ~ SMOKER3
    ) %>% as.factor(),
    RACE = case_when(
      RACE == 9 ~ NA_real_,
      TRUE ~ RACE
    ),
    RACE = as.character(RACE),
    RACE = case_when(
      RACE %in% names(race_labels) ~ RACE,
      is.na(RACE) ~ "Unknown",
      TRUE ~ RACE
    ),
    INCOME = case_when(
      INCOME %in% c(77, 99) ~ NA_real_, 
      INCOME %in% c(8, 9, 10, 11) ~ 8,  # Collapsing categories
      TRUE ~ INCOME
    ) %>% as.factor()
  )

# Calculate sample sizes by year
year_sample_sizes <- df %>%
  count(year, name = "sample_size")

# Calculate proportions for each year
year_sample_sizes <- year_sample_sizes %>%
  mutate(proportion = sample_size / sum(sample_size))

# Merge with main dataset
df <- df %>%
  left_join(year_sample_sizes, by = "year") %>%
  mutate(weight_adjusted = LLCPWT * proportion) %>%
  select(-sample_size, -proportion)  # Remove extra columns after adjustment

# Remove participants under age 50 and drop unused factor levels
df <- df %>%
  filter(!(AGEG5YR %in% c("1", "2", "3", "4", "5", "6"))) %>%
  mutate(AGEG5YR = droplevels(AGEG5YR))

# Ensure categorical variables have correct factor levels before imputation
df <- df %>%
  mutate(
    SEXVAR = factor(SEXVAR, labels = c("Male", "Female")),
    AGEG5YR = factor(AGEG5YR, labels = c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
    MEMLOSS = factor(MEMLOSS, labels = c("Yes", "No"))
  )

# Reassign factor levels properly
df <- df %>%
  mutate(RACE = factor(RACE, levels = names(race_labels), labels = race_labels))

# Verify 
print(str(df$RACE))
print(table(df$RACE, useNA = "ifany"))

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save cleaned dataset
saveRDS(df, file.path(processed_data_dir, "02_cleaned_data.rds"))

