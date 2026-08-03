# Load required libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(patchwork)

# Assuming your data is already loaded as FM_final
data <- FM_final

# Define a custom plotting function with outlier removal and improved aesthetics
create_violin_plot <- function(data, yvar, ylab, xlab) {
  # Remove outliers using IQR method
  Q1 <- quantile(data[[yvar]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[yvar]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  clean_data <- data %>% filter(.data[[yvar]] >= lower & .data[[yvar]] <= upper)
  
  # Create the violin plot
  ggplot(clean_data, aes(x = "", y = .data[[yvar]])) +
    geom_violin(fill = "#20B2AA", width = 1.2) +
    geom_boxplot(width = 0.1, color = "black", alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
    theme_classic(base_family = "Times New Roman") +
    xlab(xlab) +
    ylab(ylab) +
    theme(
      plot.title = element_text(size = 14),
      axis.text.x = element_text(size = 14,  family = "Times New Roman"),
      axis.text.y = element_text(size = 14,  family = "Times New Roman"),
      axis.title.x = element_text(size = 14,  family = "Times New Roman"),
      axis.title.y = element_text(size = 14,  family = "Times New Roman"),
      axis.ticks = element_line(size = 0.8),
      axis.line = element_line(size = 0.8)
    )
}

# Create plots for each trait
plot_PH  <- create_violin_plot(data, "PH", "cm", "Plant Height")
plot_NOF <- create_violin_plot(data, "NF", "number", "Number Of Fingers")
plot_ET  <- create_violin_plot(data, "ET", "number", "Effective Tillers")
plot_FW  <- create_violin_plot(data, "FW", "cm", "Finger Width")
plot_FLA <- create_violin_plot(data, "FLA", "cm²", "Flag Leaf Area")
plot_YH  <- create_violin_plot(data, "YPH", "g", "Yield Per Head")
plot_DF  <- create_violin_plot(data, "Days 50% Flowering", "days", "Days to 50% Flowering")
plot_DM  <- create_violin_plot(data, "Days 80% Maturity", "days", "Days to 80% Maturity")
plot_EHL <- create_violin_plot(data, "EHL", "cm", "Ear Head Length")
plot_EHW <- create_violin_plot(data, "EHW", "g", "Ear Head Weight")
plot_TSW <- create_violin_plot(data, "TSW", "g", "Thousand Seed Weight")
plot_GY  <- create_violin_plot(data, "YL", "ton/ha", "Grain Yield")
plot_BY  <- create_violin_plot(data, "BY", "ton/ha", "Biological Yield")
plot_HI  <- create_violin_plot(data, "HI", "%", "Harvest Index")

# Combine all plots using patchwork
Final <- plot_PH + plot_ET + plot_FLA + plot_DF + plot_DM +
  plot_EHL + plot_FW + plot_NOF + plot_EHW + plot_TSW +
  plot_YH + plot_GY + plot_BY + plot_HI + plot_layout(nrow = 3, ncol = 5)

# Display the final combined plot
Final

# Save the plot with high resolution
ggsave(filename = "VIOLIN_P22LOT.png", plot = Final, width = 40, height = 28, dpi = 300, units = "cm")
