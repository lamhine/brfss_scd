# 05_visualize_data.R
# Purpose: Visualize survey-weighted SCD prevalence estimates and create Table 1

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(tidyverse)
library(survey)
library(gtsummary)
library(gt)

# Load configuration and setup files
source("config.R")
source("setup.R")

# ---------------------- #
# LOAD AND ANALYZE DATA
# ---------------------- #

# Load cleaned dataset, imputed survey designs, and final prevalence table
df <- readRDS(file.path(processed_data_dir, "02_cleaned_data.rds"))
survey_designs <- readRDS(file.path(processed_data_dir, "04A_survey_designs.rds"))
final_combined_df <- readRDS(file.path(processed_data_dir, "04B_summary_results.rds"))

# ---------------------- #
# TABLE 1A: IMPUTED DATA
# ---------------------- #

# Extract first imputed dataset
imputed_design <- survey_designs[[1]]

# Convert MEMLOSS back to factor for proper display in Table 1B
imputed_design$variables <- imputed_design$variables %>%
  mutate(MEMLOSS = factor(MEMLOSS, levels = c(1, 0), labels = c("Yes", "No")))

# Generate table of imputed data
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

# Print table
table1_imputed

# ---------------------- #
# TABLE 1B: COMPLETE CASE 
# ---------------------- #

df <- df %>%
  mutate(RACE = fct_na_value_to_level(RACE, level = "Missing"))  # Convert NA to "Missing"

# Create survey design without dropping missing data
complete_case_design <- svydesign(
  id = ~1,
  strata = ~STSTR,
  weights = ~weight_adjusted,
  data = df
)

# Generate table of complete case data table
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

# Print table
table1_complete

# ---------------------- #
# TABLE 2: ADJUSTED PREVALENCE
# ---------------------- #

# Prepare data for Table 2 in wide format
table2 <- final_combined_df %>%
  filter(type == "Adjusted") %>%
  select(RACE, weighted_prevalence, lower_ci, upper_ci) %>%
  mutate(
    weighted_prevalence = scales::percent(weighted_prevalence, accuracy = 0.1),
    lower_ci = scales::percent(lower_ci, accuracy = 0.1),
    upper_ci = scales::percent(upper_ci, accuracy = 0.1),
    Estimate = paste0(weighted_prevalence, " (", lower_ci, " – ", upper_ci, ")")
  ) %>%
  select(RACE, Estimate) %>%
  pivot_wider(names_from = RACE, values_from = Estimate) %>%
  select(`Overall\n(N=546,371)`, everything())  # Move Overall to the first column

# Create Table 2
table2_gt <- table2 %>%
  gt() %>%
  tab_header(
    title = md("**Table 2: Sex- and Age-Adjusted Prevalence of Subjective Cognitive Decline**"),
    subtitle = "Weighted prevalence and 95% confidence intervals"
  ) %>%
  cols_label(
    `Overall\n(N=546,371)` = "Overall",
    `AIAN\n(N=7,278)` = "AIAN",
    `Asian\n(N=9,216)` = "Asian",
    `Black\n(N=42,048)` = "Black",
    `Hispanic\n(N=29,605)` = "Hispanic",
    `Multiracial\n(N=9,572)` = "Multiracial",
    `NHPI\n(N=1,520)` = "NHPI",
    `Other race\n(N=3,691)` = "Other",
    `White\n(N=443,440)` = "White"
  ) %>%
  cols_align(align = "center") %>%
  tab_options(
    table.width = pct(100),
    column_labels.font.weight = "bold"
  )

# Print table
table2_gt

# ---------------------- #
# SAVE TABLES IN A SINGLE DOCX FILE FOR MANUSCRIPT
# ---------------------- #

# Define output path
tables_docx_path <- file.path(results_dir, "SCD_Tables.docx")

# 1. Merge Table 1A and Table 1B into a single gtsummary object
tbl_stack <- tbl_merge(
  tbls = list(table1_complete, table1_imputed),
  tab_spanner = c("**Table 1A: Complete Case Analysis**", "**Table 1B: Imputed Data Analysis**")
)

# 2. Convert to `gt` format and save as Word document
tbl_stack %>%
  as_gt() %>%
  gtsave(tables_docx_path)

# 3. Append Table 2 (which is already a gt object) to the same document
table2_gt %>%
  gtsave(tables_docx_path, append = TRUE)

message("Saved tables to: ", tables_docx_path)


# ---------------------- #
# FIGURE 1: ADJUSTED PREVALENCE
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
# SAVE FILES TO RESULTS DIRECTORY
# ---------------------- #

saveRDS(table1_imputed, file.path(results_dir, "table_1A.rds"))
saveRDS(table1_complete, file.path(results_dir, "table_1B.rds"))
saveRDS(table2_gt, file.path(results_dir, "table_2.rds"))
table1_imputed %>% as_gt() %>% gtsave(filename = file.path(results_dir, "table_1A.docx"))
table1_complete %>% as_gt() %>% gtsave(filename = file.path(results_dir, "table_1B.docx"))
gtsave(table2_gt, filename = file.path(results_dir, "table_2.docx"))
ggsave(filename = file.path(results_dir, "figure_1.png"),
       plot = plot_prevalence, width = 12, height = 5, dpi = 300)
