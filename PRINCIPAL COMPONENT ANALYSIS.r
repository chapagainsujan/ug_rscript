# Load required libraries
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggrepel)

# Assign dataset
FM_final_pca <- rawdata

# -----------------------------
# 1. PCA ANALYSIS
# -----------------------------
# Ensure numeric columns only
pca_result <- prcomp(FM_final_pca[, 2:6], scale. = TRUE)
print(pca_result)

# PCA summary
pca_summary <- summary(pca_result)
print(pca_summary)

# -----------------------------
# 2. SCREE PLOT
# -----------------------------
scree_plot <- fviz_eig(pca_result, addlabels = TRUE,
                       barfill = "steelblue",
                       barcolor = "steelblue")
print(scree_plot)

# Eigenvalues
eig.val <- get_eigenvalue(pca_result)
print(eig.val)

# -----------------------------
# 3. VARIABLE ANALYSIS
# -----------------------------
# Contribution
var_contrib <- fviz_contrib(pca_result, choice = "var",
                            axes = 1:2, top = 10)
print(var_contrib)

# Correlation circle
var_cor <- fviz_pca_var(pca_result,
                        col.var = "contrib",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE)
print(var_cor)

# cos2 (quality)
var_cos2 <- fviz_cos2(pca_result, choice = "var",
                      axes = 1:2)
print(var_cos2)

# -----------------------------
# 4. PCA BIPLOT
# -----------------------------
# FIX 1: Check column name exists
# Replace "Landraces" with correct column if needed
geno_name <- FM_final_pca$GEN 

# FIX 2: Handle missing values
FM_final_pca <- na.omit(FM_final_pca)

# PCA scores
pca_scores <- as.data.frame(pca_result$x)
pca_scores$geno_name <- geno_name

# Biplot
biplot_pca <- fviz_pca_biplot(pca_result,
                              geom.ind = "point",
                              col.ind = "orange",
                              col.var = "red",
                              repel = TRUE,
                              ggtheme = theme_bw()) +
  
  geom_point(data = pca_scores,
             aes(x = PC1, y = PC2),
             size = 3, shape = 21,
             fill = "skyblue", color = "black") +
  
  geom_text_repel(data = pca_scores,
                  aes(x = PC1, y = PC2, label = geno_name),
                  size = 4, color = "#FF5A00") +
  
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  
  labs(title = "PCA Biplot")

print(biplot_pca)