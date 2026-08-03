# ── Load Required Libraries ─────────────────────────────
library(tidyverse)
library(patchwork)
library(extrafont)
library(grid)

# ── Optional: Load Times New Roman font (run once) ─────
# font_import(pattern = "Times New Roman", prompt = FALSE)
# loadfonts(device = "win")

# Consistent theme settings
base_font <- "Times New Roman"
title_size <- 26
axis_text_size <- 22
axis_title_size <- 19

# Common theme
common_theme <- theme_minimal(base_family = base_font) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = axis_text_size, color = "black",
                               margin = margin(t = 0)),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.y = element_text(size = axis_title_size, color = "black"),
    plot.title = element_text(size = title_size, face = "bold"),
    panel.grid = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  )

# ── 1. Prepare Yield Summary Data ───────────────────────
summary_gyi <- GE %>%
  group_by(Genotype) %>%
  summarise(
    MeanYield = mean(Gyi, na.rm = TRUE),
    SE = sd(Gyi, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(desc(MeanYield)) %>%
  mutate(Genotype = factor(Genotype, levels = Genotype),
         Index = as.numeric(Genotype))

y_max_gyi <- max(summary_gyi$MeanYield + summary_gyi$SE) * 1.05
y_tick_end_gyi <- -max(summary_gyi$MeanYield) * 0.025
n_geno_gyi <- length(unique(summary_gyi$Genotype))

p_gyi <- ggplot(summary_gyi, aes(x = Genotype, y = MeanYield)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_errorbar(aes(ymin = MeanYield - SE, ymax = MeanYield + SE),
                width = 0.2, color = "gray60", size = 0.4) +
  geom_segment(aes(x = Index, xend = Index, y = 0, yend = y_tick_end_gyi),
               color = "black", size = 0.3) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = y_max_gyi, color = "black", size = 0.6,
           arrow = arrow(length = unit(0.25, "cm"), ends = "last", type = "closed")) +
  annotate("segment", x = 0.4, xend = n_geno_gyi + 0.6, y = 0, yend = 0, color = "black", size = 0.6) +
  labs(title = "a)", x = "", y = "Yield (t/ha)") +
  common_theme +
  scale_y_continuous(limits = c(y_tick_end_gyi, y_max_gyi), expand = c(0, 0)) +
  coord_cartesian(clip = "off")

summary_gyd <- GE %>%
  group_by(Genotype) %>%
  summarise(
    MeanYield = mean(Gyd, na.rm = TRUE),
    SE = sd(Gyd, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(desc(MeanYield)) %>%
  mutate(Genotype = factor(Genotype, levels = Genotype),
         Index = as.numeric(Genotype))

y_max_gyd <- max(summary_gyd$MeanYield + summary_gyd$SE) * 1.05
y_tick_end_gyd <- -max(summary_gyd$MeanYield) * 0.025
n_geno_gyd <- length(unique(summary_gyd$Genotype))

p_gyd <- ggplot(summary_gyd, aes(x = Genotype, y = MeanYield)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_errorbar(aes(ymin = MeanYield - SE, ymax = MeanYield + SE),
                width = 0.2, color = "gray60", size = 0.4) +
  geom_segment(aes(x = Index, xend = Index, y = 0, yend = y_tick_end_gyd),
               color = "black", size = 0.3) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = y_max_gyd, color = "black", size = 0.6,
           arrow = arrow(length = unit(0.25, "cm"), ends = "last", type = "closed")) +
  annotate("segment", x = 0.4, xend = n_geno_gyd + 0.6, y = 0, yend = 0, color = "black", size = 0.6) +
  labs(title = "b)", x = "", y = "Yield (t/ha)") +
  common_theme +
  scale_y_continuous(limits = c(y_tick_end_gyd, y_max_gyd), expand = c(0, 0)) +
  coord_cartesian(clip = "off")

# ── Yield Reduction and CV Plot ─────────────────────────
cv_summary <- yield %>%
  group_by(Genotype) %>%
  summarise(
    Mean_Gyi = mean(Gyi, na.rm = TRUE),
    SD_Gyi = sd(Gyi, na.rm = TRUE),
    Mean_Gyd = mean(Gyd, na.rm = TRUE),
    SD_Gyd = sd(Gyd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CV_Gyi = (SD_Gyi / Mean_Gyi) * 100,
    CV_Gyd = (SD_Gyd / Mean_Gyd) * 100,
    Mean_CV = (CV_Gyi + CV_Gyd) / 2
  )

combined_data <- gy_data %>%
  left_join(cv_summary %>% select(Genotype, Mean_CV), by = "Genotype") %>%
  mutate(
    Index_gyi = rank(-GY_reduction, ties.method = "first"),
    Index_cv = rank(-Mean_CV, ties.method = "first")
  )

y_max_gyi <- max(combined_data$GY_reduction) * 1.05
y_max_cv <- max(combined_data$Mean_CV) * 1.05
n_geno <- nrow(combined_data)

plot_yield_reduction <- ggplot(combined_data, aes(x = fct_reorder(Genotype, -GY_reduction), y = GY_reduction)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_segment(aes(x = Index_gyi, xend = Index_gyi, y = 0, yend = -2), color = "black", size = 0.3) +
  geom_segment(data = tibble(y_tick = seq(0, y_max_gyi, by = 25)),
               aes(x = 0.4, xend = 0.2, y = y_tick, yend = y_tick),
               inherit.aes = FALSE, color = "black", size = 0.3) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = y_max_gyi, color = "black", size = 0.6,
           arrow = arrow(length = unit(0.25, "cm"), ends = "last", type = "closed")) +
  annotate("segment", x = 0.4, xend = n_geno + 0.6, y = 0, yend = 0, color = "black", size = 0.6) +
  labs(title = "c)", x = "", y = "Reduction (%)") +
  common_theme +
  coord_cartesian(clip = "off")

plot_mean_cv <- ggplot(combined_data, aes(x = fct_reorder(Genotype, -Mean_CV), y = Mean_CV)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_segment(aes(x = Index_cv, xend = Index_cv, y = 0, yend = -2), color = "black", size = 0.3) +
  geom_segment(data = tibble(y_tick = seq(0, y_max_cv, by = 20)),
               aes(x = 0.4, xend = 0.2, y = y_tick, yend = y_tick),
               inherit.aes = FALSE, color = "black", size = 0.3) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = y_max_cv, color = "black", size = 0.6,
           arrow = arrow(length = unit(0.25, "cm"), ends = "last", type = "closed")) +
  annotate("segment", x = 0.4, xend = n_geno + 0.6, y = 0, yend = 0, color = "black", size = 0.6) +
  labs(title = "d)", x = "", y = "Mean CV (%)") +
  common_theme +
  coord_cartesian(clip = "off")

# ── Combine and Export ──────────────────────────────────
spacer <- ggplot() + theme_void()

final_plot <- (p_gyi / spacer / p_gyd + plot_layout(heights = c(1, 0.07, 1))) |
  (plot_yield_reduction / spacer / plot_mean_cv + plot_layout(heights = c(1, 0.07, 1)))

ggsave("Combined_Yield_CV_YieldReduction_Panel.tiff", plot = final_plot,
       width = 20, height = 15, dpi = 300, units = "in", device = "tiff", compression = "lzw")
