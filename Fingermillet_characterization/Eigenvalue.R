# Load necessary libraries
library(factoextra)
library(FactoMineR)
library(ggplot2)

# Perform PCA
pca_result <- prcomp(FM_final_pca[,2:15], scale. = TRUE)

# Compute eigenvalues (variances of each principal component)
eigenvalues <- (pca_result$sdev)^2

# Prepare data for scree plot
scree_data <- data.frame(
  PC = factor(paste0("PC", 1:length(eigenvalues)), levels = paste0("PC", 1:length(eigenvalues))),
  Eigenvalue = eigenvalues
)

# Scree plot using a bar chart (Eigenvalues)
pca_scree_plot <- ggplot(data = scree_data, aes(x = PC, y = Eigenvalue)) +
  geom_bar(stat = "identity", fill = "#12a3c7", color = "black", linewidth = 0.3, width = 0.6) +  # Thinner border
  geom_text(aes(label = round(Eigenvalue, 2)), vjust = -0.5, size = 4, family = "Times New Roman") +  # Labels
  scale_x_discrete(name = "Principal Components") +
  scale_y_continuous(name = "Eigen values") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(linetype = "dotted", color = "gray"),  # Major grid as dotted lines
    panel.grid.minor = element_line(linetype = "dotted", color = "gray"),  # Minor grid as dotted lines
    panel.border = element_blank(),  # Remove full border
    axis.line.x = element_line(color = "black", linewidth = 0.5),  # Bottom border
    axis.line.y = element_line(color = "black", linewidth = 0.5)   # Left border
  )

# Display plot
pca_scree_plot

# Save plot
ggsave(filename = "Eigenvalues_BarPlot.png", plot = pca_scree_plot, width = 15, height = 12, dpi = 400, units = "cm", bg = "white")
