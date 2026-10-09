# Load required libraries
library(dplyr)
library(ggplot2)
library(patchwork)

# Create a plotting function
create_trait_plot <- function(data, trait, y_label) {
  # Treatment-level mean (TRT mean, for red diamond)
  mean_by_trt <- aggregate(data[[trait]],
                           by = list(data$TRT),
                           FUN = mean,
                           na.rm = TRUE)
  colnames(mean_by_trt) <- c("TRT", trait)
  
  # Genotype-level means within each treatment
  genotype_means <- data %>%
    group_by(TRT, GEN) %>%
    summarise(mean_trait = mean(.data[[trait]], na.rm = TRUE), .groups = "drop")
  
  # Define custom colors
  treatment_colors <- c("FC" = "#E69F00", "S1" = "#56B4E9", "S2" = "#009E73")
  
  # Map colors to genotype points
  genotype_means$color <- treatment_colors[genotype_means$TRT]
  
  # Create the plot
  ggplot(data, aes(x = TRT, y = .data[[trait]])) +
    geom_boxplot(aes(color = TRT), fill = "white", outlier.shape = NA, size = 1) +
    
    # Genotype mean points with color mapped manually
    geom_point(data = genotype_means,
               aes(x = TRT, y = mean_trait),
               color = genotype_means$color,
               shape = 16, size = 2, inherit.aes = FALSE) +
    
    # Treatment mean as red diamond
    geom_point(data = mean_by_trt,
               aes(x = TRT, y = .data[[trait]]),
               shape = 18, color = "red", size = 2, inherit.aes = FALSE) +
    
    # Define manual colors for boxplot
    scale_color_manual(values = treatment_colors) +
    
    labs(x = "", y = y_label) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 16, family = "Times New Roman"),
      axis.title.y = element_text(size = 14, family = "Times New Roman"),
      axis.text.x = element_text(size = 14, family = "Times New Roman"),
      axis.text.y = element_text(size = 13, family = "Times New Roman"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.7),
      legend.position = "none"
    )
}

# Generate plots for each trait
RL_plot   <- create_trait_plot(SD, "RL",   "Root Length (cm)")
SL_plot   <- create_trait_plot(SD, "SL",   "Shoot Length (cm)")
RFW_plot  <- create_trait_plot(SD, "RFW",  "Root Fresh Weight (g)")
SFW_plot  <- create_trait_plot(SD, "SFW",  "Shoot Fresh Weight (g)")
SDW_plot  <- create_trait_plot(SD, "SDW",  "Shoot Dry Weight (g)")
RDW_plot  <- create_trait_plot(SD, "RDW",  "Root Dry Weight (g)")
RS_plot   <- create_trait_plot(SD, "RS",   "Root:Shoot Ratio")
SRL_plot  <- create_trait_plot(SD, "SRL",  "Specific Root Length (m g⁻¹)")

# Combine all plots
Final_plot <- RL_plot + SL_plot + RFW_plot + SFW_plot + SDW_plot + RDW_plot + RS_plot + SRL_plot +
  plot_layout(ncol = 4)

# Show combined plot
Final_plot

# Save combined plot to file
ggsave("Combined_Boxplot_RootTraits_F2C_S1_S2.tiff",
       plot   = Final_plot,
       width  = 13, height = 8, dpi = 400)
