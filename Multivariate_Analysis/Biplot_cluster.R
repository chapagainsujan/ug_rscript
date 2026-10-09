# Load libraries
library(factoextra)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(extrafont)  # For Times New Roman font

# Optional: Only run once to import fonts
# font_import(pattern = "Times New Roman", prompt = FALSE)
# loadfonts(device = "win")

# Prepare dataset
Final <- SSD

# Perform PCA
my_pca <- prcomp(Final[, 2:9], center = TRUE, scale. = TRUE)

# Get eigenvalues for axis labels and arrow scaling
eig.val <- get_eigenvalue(my_pca)
eig_vals <- eig.val$eigenvalue
scaling_factors <- sqrt(eig_vals[1:2])  # For proper arrow scaling

# K-means clustering on first two PCs
set.seed(123)
km <- kmeans(my_pca$x[, 1:2], centers = 4)
cl <- factor(km$cluster, levels = c(1, 2, 3, 4), labels = c("I", "II", "III", "IV"))  # Label as Roman numerals

# Cluster centroids
centroids <- as.data.frame(my_pca$x[, 1:2]) %>%
  mutate(Cluster = cl) %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean), .groups = "drop")

# Individual scores with genotype names
ind_scores <- as.data.frame(my_pca$x[, 1:2])
colnames(ind_scores) <- c("PC1", "PC2")
ind_scores$Cluster <- cl
ind_scores$GEN <- Final$GEN

# Variable coordinates (traits) with accurate scaling
var_coords <- as.data.frame(my_pca$rotation[, 1:2])
colnames(var_coords) <- c("PC1", "PC2")
var_coords$varnames <- rownames(var_coords)
var_coords$PC1_arrow <- var_coords$PC1 * scaling_factors[1]
var_coords$PC2_arrow <- var_coords$PC2 * scaling_factors[2]

# Manually adjust trait label positions
var_coords$PC1_shift <- c(
  var_coords$PC1_arrow[1] -0.4,
  var_coords$PC1_arrow[2] + 2.3,
  var_coords$PC1_arrow[3] + 0.8,
  var_coords$PC1_arrow[4] + 2.3,
  var_coords$PC1_arrow[5] +2,
  var_coords$PC1_arrow[6] + 2.3,
  var_coords$PC1_arrow[7] - 2.5,
  var_coords$PC1_arrow[8] - 1.7
)

var_coords$PC2_shift <- c(
  var_coords$PC2_arrow[1] + 2,
  var_coords$PC2_arrow[2] +0.9,
  var_coords$PC2_arrow[3] + 1.8,
  var_coords$PC2_arrow[4] + 1.4,
  var_coords$PC2_arrow[5] - 0.5,
  var_coords$PC2_arrow[6] + 0.4,
  var_coords$PC2_arrow[7] + 0.7,
  var_coords$PC2_arrow[8] + 2
)

# Create PCA biplot
p <- fviz_pca_biplot(
  my_pca,
  geom.ind = "point",
  habillage = ind_scores$Cluster,  # Colored by Roman-labeled clusters
  pointshape = 21,
  pointsize = 2.8,
  fill.ind = ind_scores$Cluster,
  col.ind = "black",
  col.var = "black",
  repel = FALSE,
  label = "none",  # Suppress default variable labels
  axes = c(1, 2),
  ggtheme = theme_bw(base_family = "Times New Roman")
) +
  
  # Genotype labels
  geom_text_repel(
    data = ind_scores,
    aes(x = PC1, y = PC2, label = GEN, color = Cluster),
    size = 5,
    family = "Times New Roman",
    box.padding = 0.5,
    point.padding = 0.3,
    max.overlaps = 30
  ) +
  
  # Cluster centroids
  geom_point(
    data = centroids,
    aes(PC1, PC2),
    size = 4,
    shape = 21,
    fill = "white",
    color = "black",
    stroke = 1.1
  ) +
  
  # Cluster labels (Roman numerals)
  geom_text(
    data = centroids,
    aes(PC1, PC2, label = Cluster),
    fontface = "bold",
    vjust = -1.2,
    size = 5,
    family = "Times New Roman"
  ) +
  
  # Trait labels (manually shifted positions)
  geom_text(
    data = var_coords,
    aes(x = PC1_shift, y = PC2_shift, label = varnames),
    size = 5,
    family = "Times New Roman",
    fontface = "bold"
  ) +
  
  # Central axes
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Axis labels with variance explained
  labs(
    x = paste0("PC1 (", round(eig.val$variance.percent[1], 1), "%)"),
    y = paste0("PC2 (", round(eig.val$variance.percent[2], 1), "%)"),
    fill = "Cluster",
    color = "Cluster"
  ) +
  
  # Color scales
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  
  # Theme customization
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold", family = "Times New Roman"),
    legend.text = element_text(size = 11, family = "Times New Roman"),
    axis.title = element_text(size = 14, face = "bold", family = "Times New Roman"),
    axis.text = element_text(size = 12, family = "Times New Roman"),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )+ labs(title = "PCA Biplot S1 (75% FC)")

# Save as high-resolution TIFF
tiff("PCA_S175.tiff", width = 10, height = 8, units = "in", res = 300)
print(p)
dev.off()
