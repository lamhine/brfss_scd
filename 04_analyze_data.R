# 04_analyze_data.R
# Purpose: Analyze BRFSS data for SCD prevalence, applying survey weights

# Load required packages
library(tidyverse)
library(survey)
library(mice)
library(rstudioapi)

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() || is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration
source("config.R")

# Load imputed dataset
imp <- readRDS(file.path(getwd(), "data", "BRFSS_Imputed.rds"))
imputed_data <- readRDS(file.path(getwd(), "data", "BRFSS_Imputed_Completed.rds"))

# Set survey design settings
options(survey.lonely.psu = "adjust")

# Create survey design objects for each imputed dataset
survey_designs <- lapply(imputed_data, function(data) {
  svydesign(
    id = ~1,
    strata = ~STSTR,
    weights = ~weight_adjusted,
    data = data
  )
})

## SAMPLE SIZE CALCULATIONS ##
calculate_sample_sizes_unweighted <- function(design) {
  design$variables %>%
    group_by(RACE) %>%
    summarize(N = n(), .groups = "drop") %>%
    mutate(RACE = recode(
      RACE,
      "1" = "White", "2" = "Black", "3" = "AIAN", 
      "4" = "Asian", "5" = "NHPI", "6" = "Other", 
      "7" = "Multiracial", "8" = "Hispanic"
    ))
}

combine_sample_sizes <- function(survey_designs) {
  sample_sizes <- lapply(survey_designs, calculate_sample_sizes_unweighted)
  bind_rows(sample_sizes, .id = "imputation") %>%
    group_by(RACE) %>%
    summarize(N = mean(N, na.rm = TRUE), .groups = "drop")
}

sample_sizes <- combine_sample_sizes(survey_designs)
sample_sizes <- sample_sizes %>% mutate(RACE = as.character(RACE))
overall_N <- sum(sample_sizes$N, na.rm = TRUE)

## PREVALENCE CALCULATIONS ##
calculate_crude_prevalence <- function(design) {
  by_race <- svyby(
    ~as.numeric(MEMLOSS == "1"),
    ~RACE,
    design = design,
    svymean,
    na.rm = TRUE
  ) %>%
    as_tibble() %>%
    rename(weighted_prevalence = `as.numeric(MEMLOSS == "1")`, se = se) %>%
    mutate(RACE = recode(
      RACE,
      "1" = "White", "2" = "Black", "3" = "AIAN", 
      "4" = "Asian", "5" = "NHPI", "6" = "Other", 
      "7" = "Multiracial", "8" = "Hispanic"
    ))
  
  overall <- svymean(
    ~as.numeric(MEMLOSS == "1"),
    design,
    na.rm = TRUE
  )
  
  overall <- tibble(
    RACE = "Overall",
    weighted_prevalence = coef(overall)[1],
    se = SE(overall)[1]
  )
  
  bind_rows(by_race, overall) %>%
    mutate(type = "Crude")
}

calculate_adjusted_prevalence <- function(design) {
  formula <- as.formula("as.numeric(MEMLOSS == '1') ~ RACE + AGEG5YR + SEXVAR")
  fit <- svyglm(formula, design = design, family = quasibinomial())
  
  newdata <- tibble(
    RACE = factor(levels(design$variables$RACE), levels = levels(design$variables$RACE)),
    AGEG5YR = factor(rep(levels(design$variables$AGEG5YR)[1], length(levels(design$variables$RACE))),
                     levels = levels(design$variables$AGEG5YR)),
    SEXVAR = factor(rep(levels(design$variables$SEXVAR)[1], length(levels(design$variables$RACE))),
                    levels = levels(design$variables$SEXVAR))
  )
  
  predicted_prevalence <- predict(fit, newdata = newdata, type = "response", se.fit = TRUE)
  
  by_race <- tibble(
    RACE = recode(
      levels(design$variables$RACE),
      "1" = "White", "2" = "Black", "3" = "AIAN", 
      "4" = "Asian", "5" = "NHPI", "6" = "Other", 
      "7" = "Multiracial", "8" = "Hispanic"
    ),
    weighted_prevalence = as.numeric(predicted_prevalence),
    se = sqrt(attr(predicted_prevalence, "var"))
  )
  
  overall <- tibble(
    RACE = "Overall",
    weighted_prevalence = mean(by_race$weighted_prevalence),
    se = sqrt(sum(by_race$se^2) / nrow(by_race))
  )
  
  bind_rows(by_race, overall) %>%
    mutate(
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      type = "Adjusted"
    )
}

combine_prevalence <- function(survey_designs) {
  crude_results <- lapply(survey_designs, calculate_crude_prevalence)
  adjusted_results <- lapply(survey_designs, calculate_adjusted_prevalence)
  
  combined_crude <- bind_rows(crude_results, .id = "imputation") %>%
    group_by(RACE, type) %>%
    summarize(
      weighted_prevalence = mean(weighted_prevalence, na.rm = TRUE),
      se = sqrt(sum(se^2) / n()),
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      .groups = "drop"
    )
  
  combined_adjusted <- bind_rows(adjusted_results, .id = "imputation") %>%
    group_by(RACE, type) %>%
    summarize(
      weighted_prevalence = mean(weighted_prevalence, na.rm = TRUE),
      se = sqrt(sum(se^2) / n()),
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      .groups = "drop"
    )
  
  bind_rows(combined_crude, combined_adjusted)
}

final_combined_df <- combine_prevalence(survey_designs) %>%
  mutate(RACE = as.character(RACE)) %>%
  left_join(sample_sizes, by = "RACE") %>%
  mutate(
    N = ifelse(RACE == "Overall", overall_N, N),
    RACE = ifelse(
      RACE == "Overall",
      paste0("Overall\n(N=", scales::comma(round(N, 0)), ")"),
      paste0(RACE, "\n(N=", scales::comma(round(N, 0)), ")")
    )
  )

# Save results
saveRDS(final_combined_df, file.path(getwd(), "data", "BRFSS_Results.rds"))

# Display results
print(final_combined_df)