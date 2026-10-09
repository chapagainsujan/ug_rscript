# Load required libraries
library(ggplot2)
library(reshape2)
library(openxlsx)  # For reading Excel files
library(RColorBrewer)
library(extrafont) # For Times New Roman font

# Read the dataset (modify path if necessary)
df <- FM_final_pca

# Remove the first column (Landraces) and perform PCA
df_numeric <- df[, -1]  # Exclude the Landrace column
pca_result <- prcomp(df_numeric, scale. = TRUE)

# Extract PCA loadings (coefficients)
pca_loadings <- as.data.frame(pca_result$rotation)
pca_loadings <- pca_loadings[, 1:5]  # Keep only first 5 PCs
pca_loadings$Trait <- rownames(pca_loadings)

# Convert to long format for heatmap
pca_melted <- melt(pca_loadings, id.vars = "Trait")

# Plot heatmap with text annotations
a <- ggplot(pca_melted, aes(x = variable, y = Trait, fill = value)) +
  geom_tile() +  # Heatmap tiles
  geom_text(aes(label = round(value, 2)), color = "black", size = 4, family = "Times New Roman") +  # Normal text inside boxes
  scale_fill_gradient2(low = "#f07569", mid = "white", high = "#12a3c7", midpoint = 0) +  # Color gradient
  theme_minimal() +
  labs(x = NULL, y = NULL) +  # Remove labels
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, family = "Times New Roman", color = "black", size = 10, face = "bold"),  # Bright black and bold for x-axis
    axis.text.y = element_text(family = "Times New Roman", color = "black", size = 10, face = "bold"),  # Bright black and bold for y-axis
    axis.title = element_blank(),  # Remove axis titles
    legend.position = "none"  # Remove legend
  ) +
  scale_x_discrete(position = "top") +  # Move PCs to the top
  coord_fixed()  # Ensure square-shaped tiles

# Display plot
print(a)

# Save plot
ggsave(filename = "PCA00000.png", plot = a, width = 12, height = 25, dpi = 400, units = "cm", bg = "white")
