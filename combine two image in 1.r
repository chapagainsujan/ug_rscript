library(tidyverse)
library(extrafont)
library(patchwork)  # For combining plots

# Optional: Only run once
# font_import(pattern = "Times New Roman", prompt = FALSE)
# loadfonts(device = "win")

# --- Prepare summary for Gyi (Irrigated) ---
summary_gyi <- GE %>%
  group_by(Genotype) %>%
  summarise(
    MeanYield = mean(Gyi, na.rm = TRUE),
    SE = sd(Gyi, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(desc(MeanYield))

summary_gyi$Genotype <- factor(summary_gyi$Genotype, levels = summary_gyi$Genotype)
summary_gyi$Index <- as.numeric(summary_gyi$Genotype)

y_max_gyi <- max(summary_gyi$MeanYield + summary_gyi$SE) * 1.05
y_tick_end_gyi <- -max(summary_gyi$MeanYield) * 0.035
n_geno_gyi <- length(unique(summary_gyi$Genotype))

p_gyi <- ggplot(summary_gyi, aes(x = Genotype, y = MeanYield)) +
  geom_col(fill = "steelblue", width = 0.8) +
  geom_errorbar(aes(ymin = MeanYield - SE, ymax = MeanYield + SE),
                width = 0.2, color = "gray60", size = 0.4) +
  geom_segment(aes(x = Index, xend = Index, y = 0, yend = y_tick_end_gyi),
               color = "black", size = 0.3) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = y_max_gyi,
           color = "black", size = 0.6) +
  annotate("segment", x = 0.4, xend = n_geno_gyi + 0.6, y = 0, yend = 0,
           color = "black", size = 0.6) +
  labs(title = "a) Irrigated Condition", x = "", y = "Yield (t/ha)") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,
                               family = "Times New Roman", size = 12, color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.text.y = element_text(family = "Times New Roman", size = 10, color = "black"),
    axis.title.y = element_text(family = "Times New Roman", size = 11, color = "black"),
    panel.grid = element_blank(),
    plot.title = element_text(family = "Times New Roman", size = 14, face = "bold"),
    plot.margin = margin(5, 5, 0, 5)
  ) +
  scale_y_continuous(limits = c(y_tick_end_gyi, y_max_gyi), expand = c(0, 0))

# --- Prepare summary for Gyd (Drought) ---
summary_gyd <- GE %>%
  group_by(Genotype) %>%
  summarise(
    MeanYield = mean(Gyd, na.rm = TRUE),
    SE = sd(Gyd, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(desc(MeanYield))

summary_gyd$Genotype <- factor(summary_gyd$Genotype, levels = summary_gyd$Genotype)
summary_gyd$Index <- as.numeric(summary_gyd$Genotype)

y_max_gyd <- max(summary_gyd$MeanYield + summary_gyd$SE) * 1.05
y_tick_end_gyd <- -max(summary_gyd$MeanYield) * 0.035
n_geno_gyd <- length(unique(summary_gyd$Genotype))

p_gyd <- ggplot(summary_gyd, aes(x = Genotype, y = MeanYield)) +
  geom_col(fill = "tomato3", width = 0.8) +
  geom_errorbar(aes(ymin = MeanYield - SE, ymax = MeanYield + SE),
                width = 0.2, color = "gray60", size = 0.4) +
  geom_segment(aes(x = Index, xend = Index, y = 0, yend = y_tick_end_gyd),
               color = "black", size = 0.3) +
  annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = y_max_gyd,
           color = "black", size = 0.6) +
  annotate("segment", x = 0.4, xend = n_geno_gyd + 0.6, y = 0, yend = 0,
           color = "black", size = 0.6) +
  labs(title = "b) Drought Condition", x = "", y = "Yield (t/ha)") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,
                               family = "Times New Roman", size = 12, color = "black"),
    axis.text.y = element_text(family = "Times New Roman", size = 10, color = "black"),
    axis.title.y = element_text(family = "Times New Roman", size = 11, color = "black"),
    panel.grid = element_blank(),
    plot.title = element_text(family = "Times New Roman", size = 14, face = "bold"),
    plot.margin = margin(0, 5, 5, 5)
  ) +
  scale_y_continuous(limits = c(y_tick_end_gyd, y_max_gyd), expand = c(0, 0))

# --- Create an empty spacer plot for gap ---
spacer <- ggplot() + theme_void()

# --- Combine the plots vertically with spacer to increase gap ---
combined_plot <- p_gyi / spacer / p_gyd + 
  plot_layout(heights = c(1.1, 0.15, 1.1))  # Adjust 0.25 for desired gap size

# --- Save combined plot ---
ggsave("grain_yield_irrigated_and_drought.tiff", combined_plot,
       width = 9, height = 10, dpi = 300, units = "in",
       device = "tiff", bg = "white")
