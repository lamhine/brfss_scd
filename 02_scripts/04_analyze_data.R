# 04_analyze_data.R
# Purpose: Analyze BRFSS data for SCD prevalence, applying survey weights

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(tidycensus)
library(survey)
library(mice)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD AND ANALYZE IMPUTED DATASET
# ---------------------- #

# Load imputed dataset
imputed_data <- readRDS(file.path(processed_data_dir, "03B_completed_imputations.rds"))

# Create survey design objects for each imputed dataset
survey_designs <- lapply(imputed_data, function(data) {
  data <- data %>% mutate(MEMLOSS = as.numeric(MEMLOSS == "Yes"))  # Ensure correct coding
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
    summarize(N = n(), .groups = "drop")
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

# ---------------------- #
# FUNCTIONS AND CALCULATIONS FOR CRUDE BY AGE/SEX (FIGURE 1A)
# ---------------------- #

# Function to calculate crude prevalence stratified by sex and age category
calc_crude_agesex <- function(design) {
  svyby(
    ~MEMLOSS,
    ~interaction(SEXVAR, AGEG5YR, drop = TRUE),
    design = design,
    svymean,
    na.rm = TRUE
  ) %>%
    as_tibble() %>%
    rename(weighted_prevalence = MEMLOSS, se = se) %>%
    separate(`interaction(SEXVAR, AGEG5YR, drop = TRUE)`, into = c("SEXVAR", "AGEG5YR"), sep = "\\.") %>%
    mutate(
      SEXVAR = factor(SEXVAR, levels = levels(design$variables$SEXVAR)),
      AGEG5YR = factor(AGEG5YR, levels = levels(design$variables$AGEG5YR)),
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      type = "Crude"
    )
}

# Function to combine age/sex-stratified crude across imputations 
combine_crude_agesex <- function(survey_designs) {
  # Apply the calc_crude_agesex function to each imputed dataset
  all_results <- lapply(survey_designs, calc_crude_agesex)
  
  # Combine into one dataframe with an imputation index
  combined <- bind_rows(all_results, .id = "imputation")
  
  # Pool across imputations: mean prevalence and Rubin's Rule for SE
  pooled <- combined %>%
    group_by(SEXVAR, AGEG5YR, type) %>%
    summarize(
      weighted_prevalence = mean(weighted_prevalence, na.rm = TRUE),
      se = sqrt(sum(se^2) / n()),  # Simple approximation across M imputations
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      .groups = "drop"
    )
  
  return(pooled)
}

# Dataframe of crude results stratified by age and sex (Figure 1A)
final_agesex_df <- combine_crude_agesex(survey_designs)

# ---------------------- #
# FUNCTIONS AND CALCULATIONS FOR CRUDE BY AGE/SEX/RACE (FIGURE 1B)
# ---------------------- #

# Function to calculate crude prevalence stratified by sex, age, and race
calc_crude_agesexrace <- function(design) {
  svyby(
    ~MEMLOSS,
    ~interaction(RACE, SEXVAR, AGEG5YR, drop = FALSE),
    design = design,
    svymean,
    na.rm = TRUE
  ) %>%
    as_tibble() %>%
    rename(weighted_prevalence = MEMLOSS, se = se) %>%
    separate(`interaction(RACE, SEXVAR, AGEG5YR, drop = FALSE)`, into = c("RACE", "SEXVAR", "AGEG5YR"), sep = "\\.") %>%
    mutate(
      RACE = factor(RACE, levels = levels(design$variables$RACE)),
      SEXVAR = factor(SEXVAR, levels = levels(design$variables$SEXVAR)),
      AGEG5YR = factor(AGEG5YR, levels = levels(design$variables$AGEG5YR)),
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      type = "Crude"
    )
}

# Function to combine age/sex-stratified crude across imputations 
combine_crude_agesexrace <- function(survey_designs) {
  # Apply the calc_crude_agesex function to each imputed dataset
  all_results <- lapply(survey_designs, calc_crude_agesexrace)
  
  # Combine into one dataframe with an imputation index
  combined <- bind_rows(all_results, .id = "imputation")
  
  # Pool across imputations: mean prevalence and Rubin's Rule for SE
  pooled <- combined %>%
    group_by(RACE, SEXVAR, AGEG5YR, type) %>%
    summarize(
      weighted_prevalence = mean(weighted_prevalence, na.rm = TRUE),
      se = sqrt(sum(se^2) / n()),  # Simple approximation across M imputations
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      .groups = "drop"
    )
  
  return(pooled)
}

# Dataframe of crude results stratified by age, sex, race (Figure 1B)
final_agesexrace_df <- combine_crude_agesexrace(survey_designs) %>%
  mutate(
    facet_label = paste(RACE, SEXVAR, sep = " ")
  )

# Ensure facet_label is ordered: Feale labels first, then Male
facet_levels <- c(
  "AIAN Female", "Asian Female", "Black Female", "Hispanic Female",
  "Multiracial Female", "NHPI Female", "White Female",
  "Other race Female", "Unknown Female",
  "AIAN Male", "Asian Male", "Black Male", "Hispanic Male",
  "Multiracial Male", "NHPI Male", "White Male",
  "Other race Male", "Unknown Male"
)

# Set facet ordering
final_agesexrace_df <- final_agesexrace_df %>%
  mutate(
    facet_label = paste(RACE, SEXVAR, sep = " "),
    facet_label = factor(facet_label, levels = facet_levels)
  )

# ---------------------- #
# FUNCTIONS AND CALCULATIONS FOR CRUDE AND ADJUSTED BY RACE (FIGURE 2A)
# ---------------------- #

# Function to calculate crude prevalence by race
calc_crude_race <- function(design) {
  by_race <- svyby(
    ~MEMLOSS,
    ~RACE,
    design = design,
    svymean,
    na.rm = TRUE
  ) %>%
    as_tibble() %>%
    rename(weighted_prevalence = MEMLOSS, se = se)
  
  overall <- svymean(
    ~MEMLOSS,
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

# Function to calculate age/sex-adjusted prevalence by race
calc_adj_race <- function(design) {
  formula <- as.formula("MEMLOSS ~ RACE + AGEG5YR + SEXVAR")
  fit <- svyglm(formula, design = design, family = quasibinomial())
  
  # Extract factor levels
  race_levels <- levels(design$variables$RACE)
  age_ref <- levels(design$variables$AGEG5YR)[1]  # Reference category for age
  sex_ref <- levels(design$variables$SEXVAR)[1]  # Reference category for sex
  
  newdata <- expand_grid(
    RACE = factor(race_levels, levels = race_levels),
    AGEG5YR = factor(age_ref, levels = levels(design$variables$AGEG5YR)),
    SEXVAR = factor(sex_ref, levels = levels(design$variables$SEXVAR))
  )
  
  # Run predictions
  predicted_race <- predict(fit, newdata = newdata, type = "response", se.fit = TRUE)
  
  # Extract values
  if (is.list(predicted_race)) {
    weighted_prevalence <- as.numeric(predicted_race$fit)
    se <- as.numeric(predicted_race$se.fit)
  } else {
    weighted_prevalence <- as.numeric(predicted_race)
    se <- sqrt(attr(predicted_race, "var"))  # Extract standard error
  }
  
  # Compute a properly weighted overall adjusted prevalence
  race_sample_sizes <- design$variables %>%
    group_by(RACE) %>%
    summarize(N = sum(weights(design)), .groups = "drop")
  
  overall_weighted_prevalence <- sum(weighted_prevalence * race_sample_sizes$N) / sum(race_sample_sizes$N)
  
  # Compute standard error for overall
  overall_var <- sum((se^2) * (race_sample_sizes$N / sum(race_sample_sizes$N))^2)
  overall_se <- sqrt(overall_var)
  
  # Create final output
  bind_rows(
    tibble(
      RACE = race_levels,
      weighted_prevalence = weighted_prevalence,
      se = se
    ),
    tibble(
      RACE = "Overall",
      weighted_prevalence = overall_weighted_prevalence,
      se = overall_se
    )
  ) %>%
    mutate(type = "Adjusted")
}

# Function to combine race-stratified crude + adjusted across imputations 
combine_race_results <- function(survey_designs) {
  crude_results <- lapply(survey_designs, calc_crude_race)
  adjusted_results <- lapply(survey_designs, calc_adj_race)
  
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

# Dataframe of crude and adjusted results stratified by race (Figure 2)
final_race_df <- combine_race_results(survey_designs) %>%
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

# ---------------------- #
# FUNCTIONS AND CALCULATIONS FOR TIME SERIES AND POOLED BY RACE (FIGURE 2B)
# ---------------------- #

race_year_preds <- purrr::map_dfr(seq_along(survey_designs), function(i) {
  design <- survey_designs[[i]]
  df <- design$variables
  years <- unique(df$year)
  
  purrr::map_dfr(years, function(yr) {
    this_design <- subset(design, year == yr)
    
    # Drop unused RACE levels
    observed_race_levels <- levels(droplevels(this_design$variables$RACE))
    
    newdata <- expand.grid(
      AGEG5YR = levels(this_design$variables$AGEG5YR),
      SEXVAR = levels(this_design$variables$SEXVAR),
      RACE = observed_race_levels
    )
    
    # Fit model
    formula <- MEMLOSS ~ AGEG5YR + SEXVAR + RACE
    model <- svyglm(formula, design = this_design, family = quasibinomial())
    
    # Predict
    predicted <- predict(model, newdata = newdata, type = "response", se.fit = TRUE)
    
    if (is.list(predicted)) {
      predicted_prob <- predicted$fit
      se <- predicted$se.fit
    } else {
      predicted_prob <- as.numeric(predicted)
      se <- sqrt(attr(predicted, "var"))
    }
    
    cbind(newdata,
          predicted_prob = predicted_prob,
          se = se) %>%
      as_tibble() %>%
      group_by(RACE) %>%
      summarize(
        predicted_prob = mean(predicted_prob),
        se = sqrt(mean(se^2) / n()),
        year = yr,
        .groups = "drop"
      ) %>%
      mutate(
        lower_ci = predicted_prob - 1.96 * se,
        upper_ci = predicted_prob + 1.96 * se,
        imputation = i
      )
  })
}) %>%
  group_by(RACE, year) %>%
  summarize(
    predicted_prob = mean(predicted_prob),
    se = sqrt(mean(se^2) / n()),
    lower_ci = predicted_prob - 1.96 * se,
    upper_ci = predicted_prob + 1.96 * se,
    .groups = "drop"
  )


# ---------------------- #
# FUNCTIONS AND CALCULATIONS FOR ADJUSTED STRATIFIED BY STATE (FIGURE 3)
# ---------------------- #

# Function to calculate adjusted prevalence among Multiracial adults by state
calc_adj_state_multiracial <- function(design) {
  # Filter to Multiracial adults
  design_mr <- subset(design, RACE == "Multiracial")
  
  # Fit model without STATE as a predictor
  fit <- svyglm(MEMLOSS ~ AGEG5YR + SEXVAR, design = design_mr, family = quasibinomial())
  
  # Predict probability of SCD for each person
  pred_probs <- predict(fit, type = "response")
  
  # Add predicted values back into the survey design
  design_mr$variables$pred <- as.numeric(pred_probs)
  
  # Summarize predicted prevalence within each STATE
  svyby(
    ~pred,
    ~STATE,
    design = design_mr,
    svymean,
    keep.names = FALSE,
    na.rm = TRUE
  ) %>%
    as_tibble() %>%
    rename(
      weighted_prevalence = pred,
      se = se
    )
}

# Function to combine state-stratified results across imputations 
combine_adj_state_multiracial <- function(survey_designs) {
  state_results <- lapply(survey_designs, calc_adj_state_multiracial)
  pooled <- bind_rows(state_results, .id = "imputation") %>%
    group_by(STATE) %>%
    summarize(
      weighted_prevalence = mean(weighted_prevalence, na.rm = TRUE),
      se = sqrt(sum(se^2) / n()),
      lower_ci = pmax(0, weighted_prevalence - 1.96 * se),
      upper_ci = pmin(1, weighted_prevalence + 1.96 * se),
      RSE = 100 * se / weighted_prevalence,
      unstable = any(RSE > 30),
      .groups = "drop"
    )
  return(pooled)
}

# Dataframe of state-stratified results (Figure 3)
final_state_df <- combine_adj_state_multiracial(survey_designs)

# Add state names
final_state_df <- final_state_df %>%
  mutate(STATE = str_pad(STATE, width = 2, pad = "0")) %>%
  left_join(
    fips_codes %>%
      distinct(state_code, state_name),
    by = c("STATE" = "state_code")
  )

# ---------------------- #
# SAVE FILES TO PROCESSED DATA DIRECTORY
# ---------------------- #

# Save results
saveRDS(survey_designs, file.path(processed_data_dir, "04A_survey_designs.rds"))
saveRDS(final_race_df, file.path(processed_data_dir, "04B_race_results.rds"))
saveRDS(final_agesex_df, file.path(processed_data_dir, "04C_agesex_results.rds"))
saveRDS(final_agesexrace_df, file.path(processed_data_dir, "04D_agesexrace_results.rds"))
saveRDS(final_state_df, file.path(processed_data_dir, "04E_state_results.rds"))

# Display results
print(final_agesex_df)
print(final_race_df)