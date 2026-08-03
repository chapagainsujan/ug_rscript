# Load required libraries
library(dplyr)
library(ggplot2)
library(patchwork)

# Define your dataset
SD <- Sandesh_Subedi_Wheat_Raw_Data

create_trait_plot <- function(data, trait, y_label) {
  # Treatment-level mean for black diamond
  mean_by_trt <- aggregate(data[[trait]],
                           by = list(data$TRT),
                           FUN = mean,
                           na.rm = TRUE)
  colnames(mean_by_trt) <- c("TRT", trait)
  
  # Genotype-level means within each treatment
  genotype_means <- data %>%
    group_by(TRT, GEN) %>%
    summarise(mean_trait = mean(.data[[trait]], na.rm = TRUE), .groups = "drop")
  
  ggplot(data, aes(x = TRT, y = .data[[trait]], color = TRT)) +
    geom_boxplot(fill = "white", outlier.shape = NA, size = 1) +
    
    # Add genotype mean as points
    geom_point(data = genotype_means,
               aes(x = TRT, y = mean_trait, color = TRT),
               shape = 16,   # Change shape here (16 = solid circle, 17 = triangle, etc.)
               size = 2.5,
               inherit.aes = FALSE) +
    
    # Add overall TRT mean as black diamond
    geom_point(data = mean_by_trt,
               aes(x = TRT, y = .data[[trait]]),
               shape = 18, color = "black", size = 3.5,
               inherit.aes = FALSE) +
    
    scale_color_manual(values = c("Drought" = "#E69F00", "Irrigated" = "#56B4E9")) +
    
    labs(x = "", y = y_label) +
    theme_minimal() +
    theme(
      axis.title.x  = element_text(size = 14, family = "Times New Roman"),
      axis.title.y  = element_text(size = 10, family = "Times New Roman"),
      axis.text.x   = element_text(size = 12, family = "Times New Roman"),
      axis.text.y   = element_text(size = 10, family = "Times New Roman"),
      plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
      panel.border  = element_rect(color = "black", fill = NA, size = 0.7),
      legend.position = "none"
    )
  
}
PH_plot  <- create_trait_plot(SD, "PH",  "Plant Height (cm)")
RL_plot  <- create_trait_plot(SD, "RL",  "Root Length (cm)")
TRN_plot  <- create_trait_plot(SD, "TRN","Total root number (n)")
SL_plot  <- create_trait_plot(SD, "SL",  "Spike Length (cm)")
TT_plot <- create_trait_plot(SD, "TT",  "Total Tillers (n)")
ET_plot  <- create_trait_plot(SD, "ET",  "Effective Tillers (n)")
FLA_plot <- create_trait_plot(SD, "FLA", "Flag Leaf Area (cm2)")
DTB_plot <- create_trait_plot(SD, "DTB", "Days to Booting (days)")
DTH_plot <- create_trait_plot(SD, "DTH", "Days to Heading (days)")
DTA_plot <- create_trait_plot(SD, "DTA", "Days to Anthesis (days)")
DTM_plot <- create_trait_plot(SD, "DTM", "Days to Maturity (days)")
NS_plot  <- create_trait_plot(SD, "NS",  "Number of Spikelets (n)")
NGPS_plot<- create_trait_plot(SD, "NGPS","No. of Grains/Spike (n)")
GY_plot  <- create_trait_plot(SD, "GY",  "Grain Yield (ton/ha)")

Final_plot <- PH_plot + RL_plot + TRN_plot+TT_plot+ ET_plot + FLA_plot +
  DTB_plot + DTH_plot + DTA_plot + DTM_plot +SL_plot + NS_plot +
  NGPS_plot + GY_plot + plot_layout(ncol = 4)

Final_plot

ggsave("Combined_Boxplot_WHEAT.png",
       plot   = Final_plot,
       width  = 15, height = 12, dpi = 400)
