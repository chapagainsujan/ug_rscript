# Load necessary libraries
library(GGally)
library(ggplot2)


# Select only trait columns (columns 3 to 16)
trait_data <- Millet[, 3:16]

# Custom correlation function: only numeric value + significance stars
my_clean_cor <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr_val <- cor(x, y, use = "complete.obs")
  test <- cor.test(x, y)
  stars <- symnum(test$p.value, corr = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
  label <- paste0(round(corr_val, 2), stars)
  ggplot(data = data, mapping = mapping) +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = label, size = 4, family = "Times New Roman")
}

# Create ggpairs plot
p <- ggpairs(
  data = trait_data,
  upper = list(continuous = my_clean_cor),
  lower = list(continuous = wrap("points", alpha = 0.6, size = 0.8)),
  diag = list(continuous = wrap("barDiag", colour = "blue", fill = "lightblue"))
)

# Apply Times New Roman font to everything
p <- p + theme(
  text = element_text(family = "Times New Roman", size = 12),
  strip.text = element_text(family = "Times New Roman", size = 12)
)

print(p)

# Apply Times New Roman font and add a box around the whole plot
p <- p + theme(
  text = element_text(family = "Times New Roman", size = 12),
  strip.text = element_text(family = "Times New Roman", size = 12),
  panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add border around the plot
)

# Print the plot
print(p)
# Apply Times New Roman font and add a border around the whole image (entire plot)
a <- p + theme(
  text = element_text(family = "Times New Roman", size = 12),
  strip.text = element_text(family = "Times New Roman", size = 12),
  plot.margin = margin(10, 10, 10, 10),  # Add some margin space around the plot
  panel.border = element_blank(),        # Remove individual panel borders
  plot.background = element_rect(color = "black", size = 1)  # Add a border around the whole plot
)

# Print the plot
print(a)

# Save the plot with specified width, height, dpi, and units
ggsave(
  filename = "ggpairs11_plot_with_whole_image_border.png",  # or .png/.pdf as needed
  plot = a,
  width = 25,
  height = 20,
  dpi = 400,
  units = "cm"
)


