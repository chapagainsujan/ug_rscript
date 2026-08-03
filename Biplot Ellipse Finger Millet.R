library(factoextra)
library(ggplot2)
library(ggrepel)

biplot999 <- fviz_pca_biplot(my_pca, 
                             geom.ind = "point",       # Points for individuals
                             col.ind = "skyblue",      # Individual points in light blue
                             col.var = "red",          # Variable arrows in red
                             addEllipses = TRUE,       # Add ellipses
                             ellipse.type = "confidence", # Confidence ellipses
                             ellipse.level = 0.68,     # 68% confidence level
                             axes = c(1, 2),           # Display PC1 and PC2
                             repel = TRUE,             # Avoid label overlap
                             ggtheme = theme_bw()) +   # White background with gridlines
  # Customizing point appearance
  geom_point(data = data.frame(my_pca$ind$coord, geno_name),
             aes(x = Dim.1, y = Dim.2),
             size = 4,                 # Increase point size
             shape = 21,               # Circle with border
             fill = "skyblue",         # Fill color for points
             color = "black",          # Border color
             stroke = 0.6) +           # Border thickness
  # Add text labels
  geom_text_repel(data = data.frame(my_pca$ind$coord, geno_name),
                  aes(x = Dim.1, y = Dim.2, label = geno_name),
                  size = 4, 
                  color = "orange", 
                  segment.size = 0.3, 
                  segment.color = "gray50") +
  # Adjusting the theme to match your plot
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White background
    panel.grid.major = element_line(color = "gray70", linetype = "dashed"),  # Dashed grid
    panel.grid.minor = element_blank(),  # No minor grid
    axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
    axis.text = element_text(size = 12),  # Axis tick labels
    legend.position = "none"              # No legend
  ) +
  labs(title = NULL)  # Remove title

# Print the biplot
print(biplot999)
