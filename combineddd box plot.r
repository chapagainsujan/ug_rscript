# ── 1. Load Libraries ─────────────────────────────────────────────
library(ggplot2)
library(patchwork)
library(showtext)

# ── 2. Font Setup ────────────────────────────────────────────────
font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf")
showtext_auto()

data <- Millet  # Your dataset

# ── 3. Custom Theme ──────────────────────────────────────────────
base_size <- 50    # Axis label and tick size
title_size <- 30   # Plot title size (optional)
tag_size <- 50     # Plot tag size for labels a, b, c, ...

custom_theme <- theme(
  panel.grid.major = element_line(color = "gray80", size = 0.4),
  panel.grid.minor = element_line(color = "gray90", size = 0.2),
  panel.border     = element_blank(),
  axis.line        = element_blank(),  # ❌ Removes bottom/left axis lines
  axis.title.x     = element_text(size = base_size, family = "Times New Roman"),
  axis.title.y     = element_text(size = base_size, family = "Times New Roman"),
  axis.text.x      = element_blank(),
  axis.text.y      = element_text(size = base_size, color = "black", family = "Times New Roman"),
  plot.title       = element_text(size = title_size, hjust = 0.5, family = "Times New Roman")
)

# ── 4. Boxplot Function ──────────────────────────────────────────
make_boxplot <- function(data, trait, xlab_text, ylab_text) {
  mean_overall <- mean(data[[trait]], na.rm = TRUE)
  mean_by_landrace <- aggregate(data[[trait]], list(data$Landraces), mean)
  colnames(mean_by_landrace) <- c("Landraces", trait)
  
  ggplot(data, aes(x = "", y = .data[[trait]])) +
    geom_boxplot(color = "#E69F00", fill = "white", outlier.shape = NA, size = 0.8, lwd = 0.8) +
    geom_point(data = mean_by_landrace, aes(x = "", y = .data[[trait]]),
               shape = 18, color = "#E69F00", size = 3.1) +
    annotate("point", x = 1, y = mean_overall, shape = 16, color = "black", size = 2.8) +
    xlab(xlab_text) + ylab(ylab_text) +
    theme_grey() + custom_theme
}

# ── 5. Generate All Boxplots ─────────────────────────────────────
PH_plot   <- make_boxplot(Millet, "PH", "PH", "cm")
ET_plot   <- make_boxplot(Millet, "ET", "ET", "no.")
FLA_plot  <- make_boxplot(Millet, "FLA", "FLA", "cm²")
DTF_plot  <- make_boxplot(Millet, "DTF", "DTF", "days")
DTM_plot  <- make_boxplot(Millet, "DTM", "DTM", "days")
EHL_plot  <- make_boxplot(Millet, "EHL", "EHL", "cm")
FW_plot   <- make_boxplot(Millet, "FW", "FW", "cm")
NF_plot   <- make_boxplot(Millet, "NF", "NF", "n")
EHW_plot  <- make_boxplot(Millet, "EHW", "EHW", "g")
TSW_plot  <- make_boxplot(Millet, "TSW", "TSW", "g")
YPH_plot  <- make_boxplot(Millet, "YPH", "YPH", "g")
BY_plot   <- make_boxplot(Millet, "BY", "BY", "t/ha")
HI_plot   <- make_boxplot(Millet, "HI", "HI", "%")
GY_plot   <- make_boxplot(Millet, "GY", "GY","t/ha")

# ── 6. Combine Plots and Apply Tag Theme ─────────────────────────
Final <- (
  PH_plot + ET_plot + FLA_plot + DTF_plot + DTM_plot +
    EHL_plot + FW_plot + NF_plot + EHW_plot + TSW_plot +
    YPH_plot + BY_plot + HI_plot + GY_plot
) +
  plot_layout(nrow = 3, ncol = 5) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "",
    tag_suffix = "."
  ) &
  theme(
    plot.tag = element_text(size = tag_size, family = "Times New Roman", face = "bold")
  )

# ── 7. Save Output Image ─────────────────────────────────────────
ggsave(
  filename = "Boxplot_sujanmllet.tiff",
  plot = Final,
  width = 35, height = 25, dpi = 300, units = "cm"
)

# ── 8. Show the Plot ─────────────────────────────────────────────
Final
