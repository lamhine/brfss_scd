# 05_visualize_data.R
# Purpose: Visualize survey-weighted SCD prevalence estimates and create Tables

# ---------------------- #
# LOAD PACKAGES AND CONFIGURATION
# ---------------------- #

# Load required packages
library(survey)
library(tidyverse)
library(maps)
library(usmap)
library(ggrepel)
library(ggforce)
library(patchwork)
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
final_race_df <- readRDS(file.path(processed_data_dir, "04B_race_results.rds"))
final_agesex_df <- readRDS(file.path(processed_data_dir, "04C_agesex_results.rds"))
final_agesexrace_df <- readRDS(file.path(processed_data_dir, "04D_agesexrace_results.rds"))
final_state_df <- readRDS(file.path(processed_data_dir, "04E_state_results.rds"))

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
# FIGURE 1A: CRUDE PREVALENCE STRATIFIED BY SEX AND AGE GROUP
# ---------------------- #
# Create plot of crude prevalence by age and sex
plot_agesex <- ggplot(
  mutate(final_agesex_df, SEXVAR = factor(SEXVAR, levels = c("Female", "Male"))),
  aes(
    x = AGEG5YR,
    y = weighted_prevalence,
    color = AGEG5YR
  )
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0
  ) +
  geom_text_repel(
    aes(label = scales::percent(weighted_prevalence, accuracy = 0.1)),
    size = 3,
    color = "black",
    force = 15,
    box.padding = 0.4,
    segment.color = "gray60",
    segment.size = 0.5
  ) +
  facet_wrap(~SEXVAR, nrow = 1) +
  labs(
    x = "Age Group",
    y = "Prevalence (%)"
  ) +
  scale_y_continuous(
    limits = c(0.1, 0.2),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  guides(color = "none") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# View plot
plot_agesex

# ---------------------- #
# FIGURE 1B: CRUDE PREVALENCE STRATIFIED BY SEX, AGE GROUP, RACE
# ---------------------- #

# Create female only age/race stratified plot
plot_female <- ggplot(
  final_agesexrace_df %>% filter(SEXVAR == "Female"),
  aes(x = AGEG5YR, y = weighted_prevalence, color = AGEG5YR)
) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0) +
  geom_text_repel(
    aes(label = scales::percent(weighted_prevalence, accuracy = 0.1)),
    size = 3, color = "black", force = 15, box.padding = 0.4,
    segment.color = "gray60", segment.size = 0.5
  ) +
  facet_wrap(~facet_label, nrow = 3) +
  labs(x = "Age Group", y = "Prevalence (%)") +
  scale_y_continuous(
    limits = c(0, 0.6),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  guides(color = "none") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# Create male only age/race stratified plot
plot_male <- ggplot(
  final_agesexrace_df %>% filter(SEXVAR == "Male"),
  aes(x = AGEG5YR, y = weighted_prevalence, color = AGEG5YR)
) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0) +
  geom_text_repel(
    aes(label = scales::percent(weighted_prevalence, accuracy = 0.1)),
    size = 3, color = "black", force = 15, box.padding = 0.4,
    segment.color = "gray60", segment.size = 0.5
  ) +
  facet_wrap(~facet_label, nrow = 3) +
  labs(x = "Age Group") +
  scale_y_continuous(
    limits = c(0, 0.6),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  guides(color = "none") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# Combine female and male plots
plot_agesexrace <- plot_female + plot_male +
  plot_layout(ncol = 2, widths = c(1, 1)) &
  theme(legend.position = "none")

# Combine plots 1A and 1B
plot_agesexracecombined <- plot_agesex / plot_agesexrace +
  plot_layout(heights = c(1, 2)) 

# View plot
plot_agesexracecombined

# ---------------------- #
# FIGURE 2A: CRUDE AND ADJUSTED PREVALENCE STRATIFIED BY RACE
# ---------------------- #

# Create prevalence plot with improved labels
plot_race <- ggplot(
  mutate(final_race_df, type = factor(type, levels = c("Crude", "Adjusted"))),
  aes(
    x = fct_reorder(RACE, weighted_prevalence, .fun = min, .desc = FALSE), 
    y = weighted_prevalence, 
    group = type, 
    color = fct_reorder(RACE, weighted_prevalence, .fun = min, .desc = FALSE)
  ) 
) +
  geom_point(aes(shape = type), size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, position = position_dodge(width = 0.5)) +
  
  # Add repelled text labels with black text and gray connectors
  geom_text_repel(
    aes(label = scales::percent(weighted_prevalence, accuracy = 0.1)),
    position = position_dodge(width = 0.5),  # Align with points
    size = 4,  # Matches axis font size (~12pt in ggplot)
    color = "black",  # Ensure labels are black
    force = 15,  # Increase repelling force
    box.padding = 0.4,  # More space between text and points
    segment.color = "gray60",  # Light gray connector lines
    segment.size = 0.5  # Thin connector lines
  ) +
  
  # Labels
  labs(
    x = NULL,
    y = "Prevalence (%)",
    shape = "Estimate Type"
  ) +
  
  # Adjust legend markers
  scale_shape_manual(
    values = c("Crude" = 16, "Adjusted" = 17),
    labels = c("Crude", "Adjusted (sex, age)")
  ) +
  
  # Adjust y-axis
  scale_y_continuous(
    limits = c(0.05, 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # Adjust legend & theme
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

# View plot
plot_race

# ---------------------- #
# FIGURE 2B: TIME SERIES AND POOLED PLOT TOGETHER
# ---------------------- #
pooled_race_df <- final_race_df %>%
  filter(type == "Adjusted") %>%
  mutate(RACE = sub("\n\\(.*$", "", RACE))

race_order <- c(
  "Asian",
  "White",
  "Unknown",
  "Black",
  "NHPI",
  "Hispanic",
  "Overall",
  "Other race",
  "Multiracial",
  "AIAN"
)

race_year_preds <- race_year_preds %>%
  mutate(RACE = factor(RACE, levels = race_order))

# Create the time series plot
plot_time <- ggplot(race_year_preds, aes(x = year, y = predicted_prob, color = RACE)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = RACE), alpha = 0.2, color = NA) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.05, 0.25)) +
  labs(
    x = "Year",
    y = "Age- and Sex-Adjusted Prevalence",
    color = "Race/Ethnicity",
    fill = "Race/Ethnicity"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  ) +
  guides(color = "none", fill = "none")

# View the plot
plot_time

# Apply levels and arrange for plotting left-to-right by prevalence
pooled_race_df <- pooled_race_df %>%
  mutate(
    RACE = factor(RACE, levels = race_order)
  ) %>%
  arrange(weighted_prevalence) %>%
  mutate(x_pos = factor(RACE, levels = RACE))  # for plotting left-to-right

# Create the ordered pooled prevalence plot
plot_overall_ordered <- ggplot(pooled_race_df, aes(x = fct_reorder(RACE, weighted_prevalence), y = weighted_prevalence)) +
  geom_point(aes(color = RACE), size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = RACE), width = 0.1) +
  geom_text_repel(
    aes(label = scales::percent(weighted_prevalence, accuracy = 0.1)),
    color = "black",              
    nudge_y = 0.005,              
    size = 3.5,
    direction = "y",
    segment.color = "gray60",     
    segment.size = 0.4,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    limits = c(0.05, 0.25),
    labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Overall", y = NULL, color = "Race or Ethnicity") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# View the plot
plot_overall_ordered

# Combine plots
plot_combined <- plot_time + plot_overall_ordered +
  plot_layout(guides = "collect", widths = c(3, 1)) &
  theme(legend.position = "right")

# View the plot
plot_combined


# ---------------------- #
# FIGURE 3: ADJUSTED PREVALENCE MAP
# ---------------------- #
# Add 2-letter state codes for joining
state_abbr_lookup <- tibble(
  state_name = state.name,
  state = state.abb  # 'state' is the column name used by plot_usmap()
) %>%
  add_row(state_name = "District of Columbia", state = "DC")

# Clean and join data
final_state_df_clean <- final_state_df %>%
  left_join(state_abbr_lookup, by = "state_name") %>%
  filter(!is.na(state))  # Remove rows without 2-letter abbreviation

# Plot using plot_usmap
map_plot <- plot_usmap(
  regions = "states",
  data = final_state_df_clean,
  values = "weighted_prevalence"
) +
  scale_fill_gradient2(
    low = "white",
    mid = "#deebf7",
    high = "#08519c",
    midpoint = 0.18,
    name = "Adjusted SCD\nPrevalence (%)",
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) 

# View the map
map_plot

# ---------------------- #
# SAVE FILES TO RESULTS DIRECTORY
# ---------------------- #

saveRDS(table1_imputed, file.path(results_dir, "table_1A.rds"))
saveRDS(table1_complete, file.path(results_dir, "table_1B.rds"))
table1_imputed %>% as_gt() %>% gtsave(filename = file.path(results_dir, "table_1A.docx"))
table1_complete %>% as_gt() %>% gtsave(filename = file.path(results_dir, "table_1B.docx"))
ggsave(filename = file.path(results_dir, "figure_1A.pdf"),
       plot = plot_agesex, width = 12, height = 5, device = "pdf")
ggsave(filename = file.path(results_dir, "figure_1B.pdf"),
       plot = plot_agesexracecombined, width = 16, height = 12, device = "pdf")
ggsave(filename = file.path(results_dir, "figure_2A.pdf"),
       plot = plot_race, width = 12, height = 5, device = "pdf")
ggsave(filename = file.path(results_dir, "figure_2B.pdf"),
       plot = plot_combined, width = 12, height = 5, device = "pdf")
ggsave(filename = file.path(results_dir, "figure_3.pdf"),
       plot = map_plot, width = 12, height = 5, device = "pdf")
