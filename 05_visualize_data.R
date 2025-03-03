# 05_visualize_data.R
# Purpose: Create visualizations for SCD prevalence analysis

# Load required packages
library(tidyverse)

# Ensure the working directory is the RStudio Project root
if (!rstudioapi::isAvailable() || is.null(rstudioapi::getActiveProject())) {
  stop("ERROR: Please open the RStudio Project (.RProj) before running this script.")
}

# Load configuration
source("config.R")

# Load the processed prevalence estimates
final_combined_df <- readRDS(file.path(getwd(), "data", "BRFSS_Results.rds"))

# Create prevalence plot
ggplot(
  final_combined_df %>% 
    mutate(RACE = fct_reorder(RACE, weighted_prevalence, .desc = FALSE)),  # Reorder RACE by prevalence
  aes(x = RACE, y = weighted_prevalence, group = type)
) +
  # Add point markers for both series with colors by RACE and shapes by type
  geom_point(aes(shape = type, color = RACE), size = 3, position = position_dodge(width = 0.5)) +
  # Add vertical error bars without horizontal lines
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = RACE), 
                width = 0, position = position_dodge(width = 0.5)) +
  # Set axis labels
  labs(
    x = NULL,
    y = "Prevalence (%)",
    shape = "Estimate Type"
  ) +
  # Customize shapes for Crude and Adjusted series
  scale_shape_manual(
    values = c("Crude" = 16, "Adjusted" = 17),
    labels = c("Crude", "Adjusted (sex, age)")  # Update Adjusted label
  ) +
  # Adjust y-axis to start at 5% and display percentages
  scale_y_continuous(
    limits = c(0.05, 0.25),  # Start y-axis at 5%
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  # Adjust legend order: Crude first
  guides(
    shape = guide_legend(order = 1, nrow = 3, byrow = TRUE),  # Arrange legend in rows
    color = "none"
  ) +
  # Adjust theme for a clean presentation look
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),  # No rotation for x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = c(1, 0),  # Align legend at the bottom of the y-axis
    legend.justification = c(1, 0),  # Align bottom-right
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.background = element_blank(),  # Remove background and border
    panel.grid = element_blank(),  # Remove gridlines
    axis.line = element_line(color = "black")  # Retain axis lines
  )

# Save the plot
ggsave(file.path(getwd(), "figures", "SCD_Prevalence_Plot.png"), width = 10, height = 6, dpi = 300)