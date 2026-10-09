library(GGally)
library(ggplot2)
library(dplyr)

# Prepare data (assuming SD is your dataset and TRT is treatment)
trait_data <- SD %>%
  select(where(is.numeric), -REP)
trait_data$TRT <- SD$TRT

# List of trait names
traits <- colnames(trait_data)[1:(ncol(trait_data) - 1)]

# Custom function for correlation text with larger size
custom_cor <- function(data, mapping, ...) {
  ggally_cor(data = data, mapping = mapping, size = 4, ...) + 
    theme_void()
}

# Create ggpairs plot
cor_plot <- ggpairs(
  data = trait_data,
  columns = 1:(ncol(trait_data) - 1),
  mapping = aes(color = TRT),
  upper = list(continuous = wrap(custom_cor)),
  lower = list(continuous = wrap("points", size = 1, alpha = 0.6)),
  diag = list(continuous = wrap("densityDiag")),
  columnLabels = traits
)


# Apply global text theme (optional for consistency)
cor_plot <- cor_plot + theme(strip.text = element_text(size = 12, face = "bold"))

# Print the final plot
print(cor_plot)



# Save high resolution
ggsave("correlation_plot_seedling.tiff", cor_plot, width = 14, height = 10, dpi = 400)

