# Load required libraries
library(pheatmap)
library(readr)

# Prepare your STI data
STI_data <- as.data.frame(STI)
rownames(STI_data) <- STI_data$GEN
STI_data <- STI_data[, -which(names(STI_data) == "GEN")]


# Set TIFF output
tiff("STI_heatmap111.tiff", width = 2000, height = 1500, res = 300)

# Plot heatmap with original values
pheatmap(STI_matrix,
         clustering_distance_rows = "euclidean",
         clustering_method = "complete",
         cluster_cols = FALSE,
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize_row = 8,
         fontsize_col = 8,
         main = "",
         treeheight_row = 50,
         fontfamily = "Times")

dev.off()




library(pheatmap)
library(RColorBrewer)

# Prepare STI data
STI_data <- as.data.frame(STI)
rownames(STI_data) <- STI_data$GEN
STI_matrix <- as.matrix(STI_data[, !(names(STI_data) %in% "GEN")])

# Perform clustering
hc_rows <- hclust(dist(STI_matrix), method = "complete")
hc_cols <- hclust(dist(t(STI_matrix)), method = "complete")

# Cut tree into 4 clusters
row_clust <- cutree(hc_rows, k = 4)
col_clust <- cutree(hc_cols, k = 4)
roman <- c("I", "II", "III", "IV")

# Annotations
ann_row <- data.frame(Cluster = factor(roman[row_clust], levels = roman))
ann_col <- data.frame(Cluster = factor(roman[col_clust], levels = roman))
rownames(ann_row) <- rownames(STI_matrix)
rownames(ann_col) <- colnames(STI_matrix)

# Annotation colors
cluster_pal <- setNames(brewer.pal(4, "Set2"), roman)
ann_colors <- list(Cluster = cluster_pal)

# Export heatmap to TIFF
tiff("STI_heatmapCLUSTERRR.tiff", width = 2500, height = 2000, res = 300)

pheatmap(
  STI_matrix,
  cluster_rows              = hc_rows,
  cluster_cols              = hc_cols,
  cutree_rows               = 4,
  cutree_cols               = 4,
  annotation_row            = ann_row,
  annotation_col            = ann_col,
  annotation_colors         = ann_colors,
  annotation_legend         = TRUE,
  annotation_names_row      = FALSE,   # <<< hides "Cluster" row label
  annotation_names_col      = FALSE,   # <<< hides "Cluster" column label
  show_rownames             = TRUE,
  show_colnames             = TRUE,
  fontsize_row              = 8,
  fontsize_col              = 8,
  fontfamily                = "Times",
  clustering_method         = "complete",
  clustering_distance_rows  = "euclidean",
  clustering_distance_cols  = "euclidean",
  treeheight_row            = 50,
  treeheight_col            = 50,
  main                      = ""
)

dev.off()
