# Load libraries
library(tidyverse)
library(heatmaply)
library(viridis)
library(plotly)

data<- Millet
# Prepare the data by selecting only numeric columns
numeric_data <- data %>% select(-Landraces, -REP)

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")


# Dimensions for square cells
n <- ncol(cor_matrix)
cell_size <- 60  # adjust this for quality
plot_width <- n * cell_size
plot_height <- n * cell_size  # Same as width for square

# Create heatmap
heatmap_plot <- heatmaply(
  cor_matrix,
  xlab = "", ylab = "",
  main = "",
  margins = c(80, 120, 60, 60),
  grid_color = "white",
  grid_width = 0.000000001,
  titleX = FALSE,
  hide_colorbar = FALSE,
  branches_lwd = 0.1,
  fontsize_row = 14,
  fontsize_col = 14,
  labCol = colnames(numeric_data),
  labRow = colnames(numeric_data),
  colors = viridis(100),
  heatmap_layers = theme(
    axis.line = element_blank(),
    axis.text = element_text(family = "Times New Roman", color = "black", face = "bold"),
    plot.title = element_text(family = "Times New Roman", color = "black", face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(family = "Times New Roman", color = "black", face = "bold"),
    legend.text = element_text(family = "Times New Roman", color = "black")
  )
)
heatmap_plot
# install.packages("webshot")
# webshot::install_phantomjs()

# Save as HTML first
htmlwidgets::saveWidget(heatmap_plot, "correlation_heatmap.html")

webshot::webshot(
  "correlation_heatmap.html", 
  "correlation_hea.png",
  vwidth = 1000,           # Change width here
  vheight = 1000,          # Change height here
  zoom = 2                 # 2x resolution (increase for sharper images)
)

# Then capture image
webshot::webshot("correlation_heatmap.html", "correlation_heatmap.png", vwidth = plot_width, vheight = plot_height)
getwd()
