# Load required libraries
library(ggplot2)
library(dplyr)

# Create the dataset using your table's data
data <- data.frame(
  Trait = c(),
  Simpson_Index = c())

# Sort the data by Simpson Index for better visualization
data <- data %>%
  arrange(desc(Simpson_Index))

# Define custom color palette
custom_colors <- c("#F26C4F", "#2B7DBB", "#5FAACB", "#8DC6D9", "#A2D9D9", 
                   "#F2B134", "#F28705")

# Create the bar plot
simpson_plot <- ggplot(data, aes(x = reorder(Trait, Simpson_Index), y = Simpson_Index, fill = Simpson_Index)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Add black border to bars
  coord_flip() +  # Flip coordinates for horizontal bars
  scale_fill_gradientn(colors = custom_colors) +  # Custom gradient colors
  labs(
    x = "Qualitative Trait",
    y = "Simpson Index"
  ) +
  theme_gray(base_size = 14) +  # Set a grey professional theme
  theme(
    text = element_text(family = "Times New Roman", color = "black"),  # Use Times New Roman font
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),  # Bold and black y-axis labels
    axis.text.x = element_text(size = 12, color = "black", face = "bold"),  # Bold and black x-axis labels
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),  # Bold and black title
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Add border
    panel.background = element_rect(fill = "white"),  # Clear white background
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),  # Dashed grid lines
    legend.position = "right"  # Place legend on the right
  ) +
  geom_text(aes(label = round(Simpson_Index, 3)), hjust = 1.1, size = 4, color = "black")  # Normal black text for values inside bars

# Save the plot
ggsave(filename = "Simpson_Index_Plot_Final.png", plot = simpson_plot, width = 30, height = 23, dpi = 300, units = "cm")
