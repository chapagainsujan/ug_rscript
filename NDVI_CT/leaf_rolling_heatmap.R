# Load required packages
library(ggplot2)
library(readr)
library(dplyr)
library(showtext)
library(sysfonts)

# Enable and load Times New Roman font
font_add("Times New Roman", regular = "times.ttf")  # Adjust the path if necessary
showtext_auto()

# Load your dataframe
df <- Leaf_rolling  # Replace with your actual dataframe
colnames(df) <- c("Genotype", "Leaf rolling Score") 
df$Category <- "Leaf Rolling Score"

# Create heatmap plot
p <- ggplot(df, aes(x = Category, y = reorder(Genotype, `Leaf rolling Score`), fill = `Leaf rolling Score`)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#60C1FF", high = "#001C3D", name = "Leaf Rolling Score") +
  labs(x = NULL, y = "Genotype") +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    # panel.border removed
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 0, hjust = 0.5),
    axis.text.y = element_text(
      size = 29,
      colour = "black",
      margin = margin(r = -9)
    ),
    axis.title.y = element_text(size = 43, margin = margin(r = 15)),  # Adds space between axis title and text
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 25)
  ) +
  coord_fixed(ratio = 0.14) +  # Compress rectangle height
  scale_y_discrete(expand = expansion(mult = c(0, 0)))  # Remove vertical padding

# Save the plot to PNG
ggsave("heatmap_leafrollinewrg.png", p, width = 4, height = 8, dpi = 300, bg = "white")
