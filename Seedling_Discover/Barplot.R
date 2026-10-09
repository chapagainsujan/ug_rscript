# ─────────────────────────────────────────────────────────────────────────────
# 1. Packages
# ─────────────────────────────────────────────────────────────────────────────
library(tidyverse)
library(showtext)
library(patchwork)

# ─────────────────────────────────────────────────────────────────────────────
# 2. Fonts
# ─────────────────────────────────────────────────────────────────────────────
font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf")
showtext_auto()

# ─────────────────────────────────────────────────────────────────────────────
# 3. Prepare data
# ─────────────────────────────────────────────────────────────────────────────
MT <- SSD   # your dataset: GEN, TRT, REP, RL, SL, RFW, SFW

sum_mt <- MT %>%
  group_by(GEN, TRT) %>%
  summarise(across(
    c(RL, SL, RFW, SFW),
    list(mean = mean,
         se   = \(x) sd(x) / sqrt(length(x))),
    .names = "{.col}_{.fn}"
  ),
  .groups = "drop")

# ─────────────────────────────────────────────────────────────────────────────
# 4. Reusable plotting function for 3 TRT conditions
# ─────────────────────────────────────────────────────────────────────────────
make_bar_mt <- function(trait, tag_label, y_text = NULL){
  
  m  <- sym(str_c(trait, "_mean"))
  se <- sym(str_c(trait, "_se"))
  
  default_labels <- list(
    "RL"  = "Root Length (cm)",
    "SL"  = "Shoot Length (cm)",
    "RFW" = "Root Fresh Weight (g)",
    "SFW" = "Shoot Fresh Weight (g)"
  )
  
  final_y_label <- if(is.null(y_text)) default_labels[[trait]] else y_text
  
  # Suggested Y-axis limits (adjust as needed)
  y_max <- case_when(
    trait == "RL"  ~ 32,
    trait == "SL"  ~ 37,
    trait == "RFW" ~ 1.3,
    trait == "SFW" ~ 0.9,
    TRUE ~ NA_real_
  )
  
  ggplot(sum_mt, aes(x = GEN, y = !!m, fill = TRT)) +
    geom_col(position = position_dodge(width = 0.88), width = 0.93) +
    geom_errorbar(aes(ymin = !!m - !!se, ymax = !!m + !!se),
                  position = position_dodge(width = 0.8),
                  width = 0.25, linewidth = 0.45) +
    scale_fill_manual(values = c("#2565C0", "#E76F00", "#008B45"), name = NULL) +
    labs(x = NULL, y = final_y_label, tag = tag_label) +
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0))) +
    scale_x_discrete(expand = expansion(mult = c(0.01, 0.01))) +
    coord_cartesian(clip = "off") +
    theme_bw(base_family = "Times New Roman") +
    theme(
      axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1,
                                  size = 60, color = "black"),
      axis.text.y  = element_text(size = 55, color = "black"),
      axis.title.y = element_text(size = 56, color = "black"),
      legend.position = "top",
      legend.text = element_text(size = 47, color = "black"),
      plot.tag = element_text(size = 80, face = "bold",
                              family = "Times New Roman", color = "black"),
      plot.tag.position = c(0, 1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin = margin(6, 10, 6, 10)
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 5. Create individual plots
# ─────────────────────────────────────────────────────────────────────────────
p1 <- make_bar_mt("RL",  "a)")
p2 <- make_bar_mt("SL",  "b)")
p3 <- make_bar_mt("RFW", "c)")
p4 <- make_bar_mt("SFW", "d)")

# ─────────────────────────────────────────────────────────────────────────────
# 6. Combine into a single figure
# ─────────────────────────────────────────────────────────────────────────────
fig_mt <- (p1 + p2) / (p3 + p4) +
  plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10)))

# ─────────────────────────────────────────────────────────────────────────────
# 7. Export High-Resolution TIFF
# ─────────────────────────────────────────────────────────────────────────────
tiff("Figure_MT.png",
     width = 5000, height = 3200,
     res = 300)
print(fig_mt)
dev.off()
