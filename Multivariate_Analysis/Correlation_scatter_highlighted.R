# Load necessary libraries
library(GGally)
library(ggplot2)

# --------------------------
# Step 1: Subset Trait Data
# --------------------------
# Select only the trait columns (adjust column indices if needed)
trait_data <- Millet[, 3:16]  # Replace 'Millet' with your actual data frame if needed

# ----------------------------------------------------
# Step 2: Define Custom Function for Upper Correlation
# ----------------------------------------------------
my_clean_cor <- function(data, mapping, ...) {
  # Extract x and y variables
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  # Calculate correlation coefficient
  corr_val <- cor(x, y, use = "complete.obs")
  
  # Perform correlation test to get p-value
  test <- cor.test(x, y)
  p_val <- test$p.value
  
  # Determine significance stars
  stars <- symnum(p_val, corr = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
  
  # Create label text
  label <- paste0(round(corr_val, 2), stars)
  
  # Background color: only significant cells are colored
  bg_col <- "white"
  if (p_val < 0.05) {
    if (corr_val > 0) bg_col <- "lightblue"
    else bg_col <- "lightpink"
  }
  
  # Build the ggplot object
  ggplot(data = data, mapping = mapping) +
    theme_void() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = bg_col, alpha = 0.5) +
    annotate("text", x = 0.5, y = 0.5, label = label, size = 4,
             family = "Times New Roman")
}

# -----------------------------
# Step 3: Create the ggpairs Plot
# -----------------------------
p <- ggpairs(
  data = trait_data,
  upper = list(continuous = my_clean_cor),  # Custom upper panel
  lower = list(continuous = wrap("points", alpha = 0.6, size = 0.8)),  # Scatterplot
  diag = list(continuous = wrap("barDiag", colour = "blue", fill = "lightblue"))  # Diagonal histograms
)

# ---------------------------------------------
# Step 4: Apply Font and Internal Panel Styling
# ---------------------------------------------
p <- p + theme(
  text = element_text(family = "Times New Roman", size = 12),
  strip.text = element_text(family = "Times New Roman", size = 12),
  panel.border = element_rect(color = "black", fill = NA, size = 1)  # Internal borders
)

# Print the plot with internal borders
print(p)

# ----------------------------------------------------
# Step 5: Add External Border Around the Whole Plot
# ----------------------------------------------------
p_final <- p + theme(
  plot.margin = margin(10, 10, 10, 10),  # Add space around plot
  panel.border = element_blank(),       # Remove individual panel borders
  plot.background = element_rect(color = "black", size = 1)  # Outer border around whole image
)

# Print the final styled plot
print(p_final)

# ------------------------------
# Step 6: Save the Final Plot
# ------------------------------
ggsave(filename = "ggpairs_significant_correlation_highlighted.png",
       plot = p_final,
       width = 25,
       height = 20,
       dpi = 400,
       units = "cm",
       bg = "white")
