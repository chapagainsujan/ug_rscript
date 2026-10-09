# Load necessary libraries
library(ggplot2)
library(ggrepel)
library(factoextra)
library(FactoMineR)

# Load the dataset (replace with actual dataset file)
df <- FM_final_pca  # Ensure dataset is correctly loaded

# Remove non-numeric columns (Landraces)
df_pca <- df[, !(names(df) %in% c("Landraces"))]

# Perform PCA using FactoMineR for QV calculation
pca_res <- PCA(df_pca, scale.unit = TRUE, graph = FALSE)

# Extract PCA results
pca_ind <- as.data.frame(pca_res$ind$coord)  # Individual scores
pca_var <- as.data.frame(pca_res$var$coord)  # Variable loadings

# Add landrace names for labeling
pca_ind$Landraces <- df$Landraces

# Compute Quality of Representation (QV) for variables (Cos² representation)
pca_var$QV <- rowSums(pca_res$var$cos2[, 1:2])  # Sum of Cos² for first two PCs

# Compute Quality of Representation (QI) for individuals
pca_ind$QI <- rowSums(pca_res$ind$cos2[, 1:2])  # Sum of Cos² for first two PCs

# Generate PCA biplot
a <- ggplot() +
  # Plot individuals (Landraces) with size based on QI
  geom_point(data = pca_ind, aes(x = Dim.1, y = Dim.2, size = QI), 
             shape = 21, fill = "#B2BEB5", color = "#FFA500", stroke = 0.6, alpha = 0.6) +
  
  # Non-overlapping text labels for landraces
  geom_text_repel(data = pca_ind, 
                  aes(x = Dim.1, y = Dim.2, label = Landraces), 
                  color = "#FFA500", 
                  family = "Times New Roman", 
                  box.padding = 0.5, 
                  point.padding = 0.3,  
                  force = 3,  
                  direction = "both",  
                  max.overlaps = Inf,
                  segment.color = NA) +  
  
  # Plot variable vectors with color based on QV
  geom_segment(data = pca_var, 
               aes(x = 0, y = 0, xend = Dim.1 * 5, yend = Dim.2 * 5, color = QV), 
               arrow = arrow(length = unit(0.2, "inches")), 
               size = 1) +
  
  # Non-overlapping text labels for variables at arrow peaks
  geom_text_repel(data = pca_var, 
                  aes(x = Dim.1 * 5, y = Dim.2 * 5, label = rownames(pca_var)), 
                  color = "brown", size = 4, family = "Times New Roman", 
                  box.padding = 0.5, 
                  point.padding = 0.3, 
                  force = 5,  
                  direction = "both",  
                  max.overlaps = Inf, 
                  segment.color = NA) +  
  
  # Add central dotted lines
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 1) +  
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 1) +
  
  # Customize theme
  theme_minimal(base_family = "Times New Roman") +  
  scale_color_gradient(low = "#454545", high = "red") +  
  scale_size_continuous(range = c(2, 6)) +  
  labs(title = "Principal Component Analysis \nBiplot for Finger Millet Landraces and Variables",
       x = paste0("PC1 (", round(pca_res$eig[1,2], 1), "%)"),
       y = paste0("PC2 (", round(pca_res$eig[2,2], 1), "%)"),
       color = "QV", size = "QI") +  
  
  # Box around the entire plot
  theme(
    legend.position = "right",  
    legend.box = "vertical",
    text = element_text(family = "Times New Roman"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    panel.grid.major = element_line(linetype = "dotted", color = "gray"),  
    panel.grid.minor = element_line(linetype = "dotted", color = "gray")   
  ) +
  
  # Adjust legend guides
  guides(
    color = guide_colorbar(title.position = "top", title.hjust = 0.4),  
    size = guide_legend(title.position = "top", title.hjust = 0.2, 
                        override.aes = list(alpha = 1), ncol = 2, 
                        position = "bottom")  
  )

# Display plot
print(a)

# Save plot
ggsave(filename = "PCA 1 and 2.png", plot = a, width = 25, height = 20, dpi = 400, units = "cm", bg = "white")
