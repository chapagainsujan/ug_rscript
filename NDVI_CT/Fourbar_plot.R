# ── 1. Packages ─────────────────────────────────────────────────────────────
library(tidyverse)
library(showtext)
library(patchwork)

# ── 2. Fonts ─────────────────────────────────────────────────────────────────
font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf")
showtext_auto()

# ── 3. Prepare data ──────────────────────────────────────────────────────────
LEAF <- leaf   # your dataset

sum_leaf <- LEAF %>%
  group_by(GEN, TRT) %>%
  summarise(across(c(FvFm, FvFo, GY, SPAD),
                   list(mean = mean,
                        se   = \(x) sd(x) / sqrt(length(x))),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# ── 4. Reusable plotting function (now includes y-axis limits) ───────────────
make_bar_leaf <- function(trait, tag_label, y_text = NULL){
  m  <- sym(str_c(trait, "_mean"))
  se <- sym(str_c(trait, "_se"))
  
  default_labels <- list(
    "FvFm" = expression(italic(F[v]) / italic(F[m])),
    "FvFo" = expression(italic(F[v]) / italic(F[o])),
    "GY"   = "Grain Yield (t/ha)",
    "SPAD" = "SPAD index"
  )
  
  final_y_label <- if(is.null(y_text)) default_labels[[trait]] else y_text
  
  # ---- The Y-axis limit logic ----
  y_max <- case_when(
    trait == "FvFm" ~ 0.9,
    trait == "FvFo" ~ 6,
    trait == "SPAD" ~ 60,
    trait == "GY"   ~ 5,
    TRUE ~ NA_real_
  )
  
  ggplot(sum_leaf, aes(x = GEN, y = !!m, fill = TRT)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.9) +
    geom_errorbar(aes(ymin = !!m - !!se, ymax = !!m + !!se),
                  position = position_dodge(width = 0.7),
                  width = 0.2, linewidth = 0.4) +
    scale_fill_manual(values = c("#2565C0", "#E76F00"), name = NULL) +
    labs(x = NULL, y = final_y_label, tag = tag_label) +
    
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0))) +
    scale_x_discrete(expand = expansion(mult = c(0.01, 0.01))) +
    coord_cartesian(clip = "off") +
    theme_bw(base_family = "Times New Roman") +
    theme(
      axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1,
                                  size = 50, color = "black"),
      axis.text.y  = element_text(size = 50, color = "black"),
      axis.title.y = element_text(size = 50, color = "black"),
      legend.position = "top",
      legend.text = element_text(size = 50, color = "black"),
      plot.tag = element_text(size = 70, face = "bold",
                              family = "Times New Roman", color = "black"),
      plot.tag.position = c(0, 1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin = margin(6, 10, 6, 10)
    )
}

# ── 5. Create individual plots ──────────────────────────────────────────────
p1 <- make_bar_leaf("FvFm", "a)")
p2 <- make_bar_leaf("FvFo", "b)")
p3 <- make_bar_leaf("GY",   "c)")
p4 <- make_bar_leaf("SPAD", "d)")

# ── 6. Combine panels ───────────────────────────────────────────────────────
fig_leaf <- (p1 + p2) / (p3 + p4) +
  plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10)))

# ── 7. Save High-Res TIFF ───────────────────────────────────────────────────
tiff("Figure_LEAF_FvFm_FvFo_GY_SPAD1111111111.tiff",
     width = 5000, height = 3200,
     res = 300, compression = "lzw")
print(fig_leaf)
dev.off()
