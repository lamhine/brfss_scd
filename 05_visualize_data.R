# 05_visualize_data.R
# Purpose: Visualize survey-weighted SCD prevalence estimates and create Table 1

# Load required packages
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(survey)
library(forcats)  # For reordering factor levels

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() || is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration
source("config.R")

# Load cleaned dataset, imputed survey designs, and final prevalence table
df <- readRDS(file.path(getwd(), "data", "BRFSS_Cleaned.rds"))
survey_designs <- readRDS(file.path(getwd(), "data", "BRFSS_SurveyDesigns.rds"))
final_combined_df <- readRDS(file.path(getwd(), "data", "BRFSS_Results.rds"))

# ---------------------- #
# TABLE 1A: COMPLETE CASE ANALYSIS
# ---------------------- #

df <- df %>%
  mutate(RACE = fct_explicit_na(RACE, na_level = "Missing"))  # Convert NA to "Missing"

complete_case_design <- svydesign(
  id = ~1,
  strata = ~STSTR,
  weights = ~weight_adjusted,
  data = df
)

table1_complete <- tbl_svysummary(
  complete_case_design,
  by = "RACE",
  include = c("SEXVAR", "AGEG5YR", "MEMLOSS"),
  missing = "ifany"
)

# Create survey design without dropping missing data
complete_case_design <- svydesign(
  id = ~1,
  strata = ~STSTR,
  weights = ~weight_adjusted,
  data = df
)

# Generate Table 1A (Complete Cases)
table1_complete <- tbl_svysummary(
  complete_case_design,
  by = "RACE",
  include = c("SEXVAR", "AGEG5YR", "MEMLOSS"),
  label = list(
    SEXVAR ~ "Sex",
    AGEG5YR ~ "Age Category",
    MEMLOSS ~ "Subjective Cognitive Decline"
  ),
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
  type = list(MEMLOSS ~ "categorical"),
  digits = all_categorical() ~ c(0, 1),
  missing = "ifany",
  missing_stat = "{N_miss_unweighted} ({p_miss_unweighted}%)",
  missing_text = "Missing"
) %>%
  add_overall() %>%
  modify_header(label = "**Characteristic**", all_stat_cols() ~ "**{level}**, N = {n_unweighted} ({style_percent(p)}%)") %>%
  modify_spanning_header(all_stat_cols() ~ "**Race/Ethnicity**") %>%
  modify_caption("Table 1A: Unweighted N and Weighted Percentages for Key Demographics") %>%
  bold_labels()

# ---------------------- #
# TABLE 1B: IMPUTED DATA (SUPPLEMENT)
# ---------------------- #

# Extract first imputed dataset
imputed_design <- survey_designs[[1]]

# Generate Table 1B (Imputed Data)
table1_imputed <- tbl_svysummary(
  imputed_design,
  by = "RACE",
  include = c("SEXVAR", "AGEG5YR", "MEMLOSS"),
  label = list(
    SEXVAR ~ "Sex",
    AGEG5YR ~ "Age Category",
    MEMLOSS ~ "Subjective Cognitive Decline"
  ),
  statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
  type = list(MEMLOSS ~ "categorical"),
  digits = all_categorical() ~ c(0, 1),
  missing = "no"
) %>%
  add_overall() %>%
  modify_header(label = "**Characteristic**", all_stat_cols() ~ "**{level}**, N = {n_unweighted} ({style_percent(p)}%)") %>%
  modify_spanning_header(all_stat_cols() ~ "**Race/Ethnicity**") %>%
  modify_caption("Table 1B: Unweighted N and Weighted Percentages (Imputed Data)") %>%
  bold_labels()

# ---------------------- #
# PLOT PREVALENCE ESTIMATES
# ---------------------- #

# Create prevalence plot
plot_prevalence <- ggplot(
  final_combined_df,
  aes(x = fct_reorder(RACE, weighted_prevalence, .fun = min, .desc = FALSE), 
      y = weighted_prevalence, group = type, color = fct_reorder(RACE, weighted_prevalence, .fun = min, .desc = FALSE))  # Preserve original color order
) +
  geom_point(aes(shape = type), size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, position = position_dodge(width = 0.5)) +
  labs(
    x = NULL,
    y = "Prevalence (%)",
    shape = "Estimate Type"
  ) +
  scale_shape_manual(
    values = c("Crude" = 16, "Adjusted" = 17),
    labels = c("Crude", "Adjusted (sex, age)")
  ) +
  scale_y_continuous(
    limits = c(0.05, 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  guides(
    shape = guide_legend(order = 1, nrow = 3, byrow = TRUE),
    color = "none"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# ---------------------- #
# SAVE RESULTS
# ---------------------- #
saveRDS(table1_complete, file.path(getwd(), "data", "BRFSS_Table1A_Complete.rds"))
saveRDS(table1_imputed, file.path(getwd(), "data", "BRFSS_Table1B_Imputed.rds"))
ggsave(filename = file.path(getwd(), "figures", "BRFSS_SCD_Prevalence.png"),
       plot = plot_prevalence, width = 10, height = 5, dpi = 300)
