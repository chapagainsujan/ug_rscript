library(metan)
library(ggplot2)

# build GGE model
gge_model <- gge(GE, ENV, GEN, GY)

# draw the biplot
b <- plot(
  gge_model,
  col.gen       = "black",
  col.env = "brown",
  size.text.env = 6,   # environment labels
  size.text.gen = 5,   # genotype labels  ⇦ NEW
  plot_theme = theme_metan(grid = "both") +
    theme(
      text        = element_text(size = 19, family = "Times New Roman"),
      axis.text   = element_text(size = 14, family = "Times New Roman"),
      axis.title  = element_text(size = 16, family = "Times New Roman"),
      legend.text = element_text(size = 14, family = "Times New Roman"),
      legend.title= element_text(size = 15, family = "Times New Roman")
    )
)

# save
ggsave(
  "normalgge.png",
  plot   = b,
  width  = 9,
  height = 9,
  dpi    = 400
)

# Generate the Type 2 GGE plot: Mean vs Stability
b <- plot(gge_model,
          type = 2,  # Type 2: Mean vs Stability
          col.gen = "black",
          col.env = "brown",
          size.text.gen = 4,
          size.text.env = 7)

# Apply classic theme and Times New Roman font
p <- b + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 12),
    axis.title = element_text(family = "Times New Roman", size = 14),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 13),
    plot.title = element_text(family = "Times New Roman", size = 15, hjust = 0.5)
  )

# Display the plot
print(p)

# Save the plot
ggsave("Mean vs stability.tiff", plot = p, width = 9, height = 9, dpi = 400)


###library(metan)     # ≥ 1.20 uses ggrepel internally
library(ggplot2)

# 1) turn on ggrepel with `repel = TRUE`
# 2) blank‐out the title with `plot.title = element_blank()`
b <- plot(
  gge_model,
  type            = 3,
  col.gen         = "black",
  col.env         = "brown",
  shape.env       = 23,
  size.shape      = 3,
  size.shape.win  = 4,
  large_label     = 6,
  size.text.gen   = 5,
  size.text.env   = 6,
  col.stroke      = "black",
  size.stroke     = 0.3,
  repel           = TRUE        # <‑‑ avoids label overlap
)

p <- b +
  theme_classic() +
  theme(
    aspect.ratio  = 1,
    text          = element_text(family = "Times New Roman", size = 14),
    axis.text     = element_text(family = "Times New Roman", size = 12),
    axis.title    = element_text(family = "Times New Roman", size = 14),
    legend.text   = element_text(family = "Times New Roman", size = 12),
    legend.title  = element_text(family = "Times New Roman", size = 13),
    plot.title    = element_blank()   # <‑‑ removes the default title
  )

print(p)
ggsave("WHICH_WON_WHERE.tiff", p, width = 9, height = 9, dpi = 400)

library(metan)     # ≥ 1.20
library(ggplot2)

b <- plot(
  gge_model,
  type            = 4,          # Discriminativeness vs. Representativeness
  ## --- genotype aesthetics -----------------------------------
  col.gen         = "black",    # outline / stroke colour of gen symbols
  fill.gen        = "black",    # fill colour (for filled shapes 21‑25)
  size.text.gen   = 4,          # larger genotype label text
  size.shape      = 3, 
  size.text.env = 5,
  # larger symbol size (applies to both gen & env)
  ## --- environment aesthetics --------------------------------
  col.env         = "brown",
  shape.env       = 23,
  ## --- general settings --------------------------------------
  size.stroke     = 0.3,
  col.stroke      = "black",
  col.circle      = "grey40",
  col.alpha.circle = 0.8,
  size.line       = 0.5,
  axis_expand     = 1.1,
  repel           = TRUE,       # separate overlapping labels
  title           = FALSE       # suppress default GGE title
)

p <- b +
  theme_classic() +
  theme(
    aspect.ratio  = 1,
    text          = element_text(family = "Times New Roman", size = 14),
    axis.text     = element_text(family = "Times New Roman", size = 12),
    axis.title    = element_text(family = "Times New Roman", size = 14),
    legend.text   = element_text(family = "Times New Roman", size = 12),
    legend.title  = element_text(family = "Times New Roman", size = 13),
    plot.title    = element_blank()
  )

print(p)

ggsave(
  "DISCRIMINATIVENESS_REPRESENTATIVENESS.tiff",
  plot   = p,
  width  = 9, height = 9, dpi = 400
)

library(metan)     # ≥ 1.20 uses ggrepel internally
library(ggplot2)

b <- plot(
  gge_model,
  type            = 6,          # Type 6: Ranking Environment plot
  col.gen         = "black",
  col.env         = "brown",
  shape.env       = 23,
  size.shape      = 3,
  size.shape.win  = 4,
  large_label     = 6,
  size.text.gen   = 5,
  size.text.env   = 6,
  col.stroke      = "black",
  size.stroke     = 0.3,
  repel           = TRUE         # Avoid label overlap
)

p <- b +
  theme_classic() +
  theme(
    aspect.ratio  = 1,
    text          = element_text(family = "Times New Roman", size = 14),
    axis.text     = element_text(family = "Times New Roman", size = 12),
    axis.title    = element_text(family = "Times New Roman", size = 14),
    legend.text   = element_text(family = "Times New Roman", size = 12),
    legend.title  = element_text(family = "Times New Roman", size = 13),
    plot.title    = element_blank()  # Removes default title
  )

print(p)

ggsave("RANKING_ENVIRONMENT.tiff", p, width = 9, height = 9, dpi = 400)
# Load required libraries
library(metan)
library(ggplot2)
library(GGEBiplots)
library(extrafont)

# Load Times New Roman font (once per session)
# Uncomment and run once if not done before:
# font_import(pattern = "Times New Roman", prompt = FALSE)

loadfonts(device = "win")  # Use "pdf" if exporting to PDF

# Set global theme
theme_set(theme_classic(base_family = "Times New Roman"))

# Define custom theme
custom_theme <- theme_classic(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.title = element_text(size = 15, hjust = 0.5)
  )

# === Biplot Type 1: Basic ===
gge_model1 <- gge(GE, ENV, GEN, GY)
b1 <- plot(gge_model1, col.gen = "orange2", size.text.env = 2)
p1 <- arrange_ggplot(b1) + custom_theme
ggsave("BIPLOT_TYPE1.tiff", p1, width = 9, height = 9, dpi = 400)

# === Biplot Type 2: Mean performance vs. Stability ===
gge_model2 <- gge(GE, ENV, GEN, GY, svp = "genotype")
b2 <- plot(gge_model2, type = 2, col.gen = "black", col.env = "brown", axis_expand = 1)
p2 <- arrange_ggplot(b2) + custom_theme
ggsave("MEAN_STABILITY.tiff", p2, width = 9, height = 9, dpi = 400)


# === Biplot Type 3: Which-won-where ===
gge_model3 <- gge(GE, ENV, GEN, GY, svp = "symmetrical")
b3 <- plot(gge_model3, type = 3, size.shape.win = 0.5, large_label = 0.5,
           col.gen = "black", col.env = "brown", annotation = FALSE, title = FALSE)
p3 <- arrange_ggplot(b3) + custom_theme
ggsave("WHICH_WON_WHERE.tiff", p3, width = 9, height = 9, dpi = 400)



# === Biplot Type 6: Ranking environments ===
gge_model6 <- gge(GE, ENV, GEN, GY, svp = "environment")
b6 <- plot(gge_model6, type = 6, col.gen = "black", col.env = "brown", 
           size.text.env = 6, axis_expand = 1.5)
p6 <- arrange_ggplot(b6) + custom_theme
ggsave("RANKING_ENVIRONMENT.tiff", p6, width = 9, height = 9, dpi = 400)

# === Biplot Type 8: Ranking genotypes ===
gge_model8 <- gge(GE, ENV, GEN, GY, svp = "genotype")
b8 <- plot(gge_model8, type = 8, col.gen = "black", col.env = "brown", 
           size.text.gen = 4, size.text.env = 5)
p8 <- arrange_ggplot(b8) + custom_theme
ggsave("RANKING_GENOTYPES.tiff", p8, width = 9, height = 9, dpi = 400)
