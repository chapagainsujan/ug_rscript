# ── Load Libraries ─────────────────────────────────────
library(tidyverse)
library(patchwork)
library(extrafont)
library(grid)

base_font <- "Times New Roman"

common_theme <- theme_minimal(base_family = base_font) +
  theme(
    # ── Axis text ─────────────────────────────
    axis.text.x = element_text(
      angle = 90, vjust = 0.5, hjust = 1,
      size = 32, color = "black"   # 🔹 GEN labels bigger
    ),
    axis.text.y = element_text(size = 26, color = "black"),
    
    axis.title.y = element_text(size = 26, color = "black"),
    
    # ── AXIS LINES (slightly bolder) ──────────
    axis.line.x = element_line(color = "black", linewidth = 1.1),
    axis.line.y = element_line(color = "black", linewidth = 1.1),
    
    # ── AXIS TICKS ───────────────────────────
    axis.ticks.x = element_line(color = "black", linewidth = 0.9),
    axis.ticks.y = element_line(color = "black", linewidth = 0.9),
    axis.ticks.length = unit(0.30, "cm"),
    
    # ── GRID (unchanged) ─────────────────────
    panel.grid.major.y = element_line(
      linetype = "dotted", linewidth = 0.3, color = "grey60"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # ── Titles & margins ─────────────────────
    plot.title = element_text(size = 50, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )

# ── Load Data ─────────────────────────────────────────
data <- SS
data$GEN <- as.factor(data$GEN)

# ── Summary ──────────────────────────────────────────
env_summary <- data %>%
  group_by(GEN, TRT) %>%
  summarise(
    MeanGY = mean(GY, na.rm = TRUE),
    SE = sd(GY, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

irrigated <- env_summary %>% filter(TRT == "Irrigated")
drought   <- env_summary %>% filter(TRT == "Drought")

overall_mean <- data %>%
  group_by(GEN) %>%
  summarise(MeanGY = mean(GY, na.rm = TRUE), .groups = "drop")

yield_loss <- irrigated %>%
  select(GEN, Irrigated = MeanGY) %>%
  left_join(drought %>% select(GEN, Drought = MeanGY), by = "GEN") %>%
  mutate(YieldLoss = ((Irrigated - Drought) / Irrigated) * 100)

# ── Plot Function (ERROR BAR FIXED) ───────────────────
plot_with_se <- function(df, title, ylab) {
  ggplot(df, aes(x = reorder(GEN, -MeanGY), y = MeanGY)) +
    geom_col(fill = "steelblue", width = 0.75) +
    geom_errorbar(
      aes(ymin = MeanGY - SE, ymax = MeanGY + SE),
      width = 0.2,
      linewidth = 0.5,        # 🔹 MADE VISIBLE IN WORD
      color = "black"
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = title, x = "", y = ylab) +
    common_theme +
    coord_cartesian(clip = "off")
}

# ── Panels ───────────────────────────────────────────
p_a <- plot_with_se(irrigated, "a)", "Yield (t ha⁻¹)")
p_b <- plot_with_se(drought, "b)", "Yield (t ha⁻¹)")

p_c <- ggplot(overall_mean, aes(x = reorder(GEN, -MeanGY), y = MeanGY)) +
  geom_col(fill = "steelblue", width = 0.75) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "c)", x = "", y = "Mean Yield (t ha⁻¹)") +
  common_theme

p_d <- ggplot(yield_loss, aes(x = reorder(GEN, -YieldLoss), y = YieldLoss)) +
  geom_col(fill = "steelblue", width = 0.75) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "d)", x = "", y = "Yield loss (%)") +
  common_theme

final_plot <- (p_a | p_b) / (p_c | p_d)

# ── EXPORT (WORD-SAFE) ───────────────────────────────
ggsave(
  filename = "S1S.tiff",
  plot = final_plot,
  width = 34,     # 🔹 increased to prevent Word thinning
  height = 22,
  dpi = 300,
  units = "in",
  compression = "lzw"
)






# ── Load Libraries ─────────────────────────────────────
library(tidyverse)
library(patchwork)
library(extrafont)
library(grid)

base_font <- "Times New Roman"

common_theme <- theme_minimal(base_family = base_font) +
  theme(
    axis.text.x = element_text(
      angle = 90, vjust = 0.5, hjust = 1,
      size = 32, color = "black"
    ),
    axis.text.y = element_text(size = 26, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    
    axis.line.x = element_line(color = "black", linewidth = 1.1),
    axis.line.y = element_line(color = "black", linewidth = 1.1),
    
    axis.ticks.x = element_line(color = "black", linewidth = 0.9),
    axis.ticks.y = element_line(color = "black", linewidth = 0.9),
    axis.ticks.length = unit(0.30, "cm"),
    
    panel.grid.major.y = element_line(
      linetype = "dotted", linewidth = 0.3, color = "grey60"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(size = 50, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )

# ── Load Data ─────────────────────────────────────────
data <- SS
data$GEN <- as.factor(data$GEN)

# ── Summary ──────────────────────────────────────────
env_summary <- data %>%
  group_by(GEN, TRT) %>%
  summarise(
    MeanGY = mean(GY, na.rm = TRUE),
    SE = sd(GY, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

irrigated <- env_summary %>% filter(TRT == "Irrigated")
drought   <- env_summary %>% filter(TRT == "Drought")

overall_mean <- data %>%
  group_by(GEN) %>%
  summarise(MeanGY = mean(GY, na.rm = TRUE), .groups = "drop")

yield_loss <- irrigated %>%
  select(GEN, Irrigated = MeanGY) %>%
  left_join(drought %>% select(GEN, Drought = MeanGY), by = "GEN") %>%
  mutate(YieldLoss = ((Irrigated - Drought) / Irrigated) * 100)

# ── Plot Function ────────────────────────────────────
plot_with_se <- function(df, title, ylab) {
  ggplot(df, aes(x = reorder(GEN, -MeanGY), y = MeanGY)) +
    geom_col(fill = "steelblue", width = 0.75) +
    geom_errorbar(
      aes(ymin = MeanGY - SE, ymax = MeanGY + SE),
      width = 0.2,
      linewidth = 0.4,
      color = "black"
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = title, x = "", y = ylab) +
    common_theme +
    coord_cartesian(clip = "off")
}

# ── Panels a & b ─────────────────────────────────────
p_a <- plot_with_se(irrigated, "a)", "Yield (t ha⁻¹)")
p_b <- plot_with_se(drought, "b)", "Yield (t ha⁻¹)")

# ── Panel c (OFFSET INCREASED) ───────────────────────
offset_c <- max(overall_mean$MeanGY) * 0.08

p_c <- ggplot(overall_mean, aes(x = reorder(GEN, -MeanGY), y = MeanGY)) +
  geom_col(fill = "steelblue", width = 0.75) +
  geom_text(
    aes(
      y = MeanGY + offset_c,
      label = round(MeanGY, 2)
    ),
    angle = 90,
    size = 11,
    family = base_font
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(title = "c)", x = "", y = "Mean Yield (t ha⁻¹)") +
  common_theme

# ── Panel d (OFFSET INCREASED) ───────────────────────
offset_d <- max(yield_loss$YieldLoss) * 0.10

p_d <- ggplot(yield_loss, aes(x = reorder(GEN, -YieldLoss), y = YieldLoss)) +
  geom_col(fill = "steelblue", width = 0.75) +
  geom_text(
    aes(
      y = YieldLoss + offset_d,
      label = round(YieldLoss, 1)
    ),
    angle = 90,
    size = 11,
    family = base_font
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.32))) +
  labs(title = "d)", x = "", y = "Yield loss (%)") +
  common_theme

# ── Combine Panels ───────────────────────────────────
final_plot <- (p_a | p_b) / (p_c | p_d)

# ── Export ───────────────────────────────────────────
ggsave(
  filename = "S1S.tiff",
  plot = final_plot,
  width = 34,
  height = 22,
  dpi = 300,
  units = "in",
  compression = "lzw"
)
