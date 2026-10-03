# ══════════════════════════════════════════════════════════════════════════════
#  PROFESSIONAL GROUPED BAR CHART — Exact style match to target figure
#  Teal + Orange palette | CC vs FR groups | NS vs SR treatments
#  Significance brackets | Dashed group divider | 4×3 panel layout
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)   # for element_markdown if needed

# ── 1. FONTS ──────────────────────────────────────────────────────────────────
font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf",
         bold = "C:/Windows/Fonts/timesbd.ttf",
         italic = "C:/Windows/Fonts/timesi.ttf",
         bolditalic = "C:/Windows/Fonts/timesbi.ttf")
showtext_auto()
showtext_opts(dpi = 600)

# ── 2. DATA ───────────────────────────────────────────────────────────────────
# REPLACE THIS BLOCK with your actual data loading:
# data <- read.csv("your_data.csv")
# data <- Leaf
#
# Required columns: GEN (genotype), TRT (treatment: NS/SR), LOC (location: CC/FR)
# and all trait columns listed below.
#
# DEMO DATA — remove and replace with your real data:
set.seed(42)
n <- 60
data <- data.frame(
  GEN = rep(paste0("G", 1:10), each = 6),
  TRT = rep(c("NS","NS","NS","SR","SR","SR"), times = 10),
  LOC = rep(c("CC","CC","FR","CC","FR","FR"), times = 10),
  FLA  = rnorm(n, mean = c(30, 35, 25, 40)[rep(1:4, 15)], sd = 3),
  RWC  = rnorm(n, mean = c(65, 75, 55, 70)[rep(1:4, 15)], sd = 5),
  LS   = rnorm(n, mean = c(0.8, 1.0, 0.7, 0.9)[rep(1:4, 15)], sd = 0.08),
  WSD  = rnorm(n, mean = c(20, 25, 18, 22)[rep(1:4, 15)], sd = 2),
  RDW  = rnorm(n, mean = c(5, 7, 4, 6)[rep(1:4, 15)], sd = 0.5),
  RSWD = rnorm(n, mean = c(15, 18, 12, 17)[rep(1:4, 15)], sd = 1.5),
  PH   = rnorm(n, mean = c(80, 95, 70, 90)[rep(1:4, 15)], sd = 5),
  SL   = rnorm(n, mean = c(12, 15, 10, 14)[rep(1:4, 15)], sd = 1),
  NS   = rnorm(n, mean = c(18, 22, 16, 20)[rep(1:4, 15)], sd = 2),
  ST   = rnorm(n, mean = c(8, 12, 6, 10)[rep(1:4, 15)], sd = 1),
  HSW  = rnorm(n, mean = c(35, 42, 30, 40)[rep(1:4, 15)], sd = 3),
  YPP  = rnorm(n, mean = c(18, 22, 14, 20)[rep(1:4, 15)], sd = 2)
)

# ── 3. CONFIGURATION ──────────────────────────────────────────────────────────
# Traits and their axis labels (modify as needed)
traits <- c("FLA", "RWC", "LS", "WSD", "RDW", "RSWD",
            "PH", "SL", "NS", "ST", "HSW", "YPP")

trait_labels <- c(
  "FLA (cm²)",
  "RWC (%)",
  "LS (g H\u2082O cm\u207B\u00B2)",
  "WSD (%)",
  "RDW (g)",
  "RSWD (%)",
  "PH (cm)",
  "SL (cm)",
  "NS",
  "ST (%)",
  "HSW (g)",
  "YPP (g)"
)

# Panel tags: (A), (B), ... matching target image style
panel_tags <- paste0("(", LETTERS[1:length(traits)], ")")

# ── 4. EXACT COLOR PALETTE from target image ──────────────────────────────────
# Teal = NS, Orange = SR  (matched visually from target)
COL_NS <- "#2CBBAD"   # teal
COL_SR <- "#F08030"   # orange

# ── 5. GLOBAL THEME SETTINGS ──────────────────────────────────────────────────
BASE_SIZE   <- 9       # base font size (pt) for 600 dpi output
AXIS_TEXT   <- 8
AXIS_TITLE  <- 8.5
TAG_SIZE    <- 10
LEGEND_SIZE <- 7.5
BAR_WIDTH   <- 0.35    # width of each bar
BAR_GAP     <- 0.05    # gap between NS and SR within a group
GROUP_GAP   <- 1.0     # distance between CC centre and FR centre (x-axis)
BRACKET_LWD <- 0.35
ERROR_LWD   <- 0.5
ERROR_W     <- 0.08

# ── 6. FORCE NUMERIC ──────────────────────────────────────────────────────────
data[traits] <- lapply(data[traits], function(x) as.numeric(as.character(x)))

# ── 7. BUILD POSITION MAPPING ─────────────────────────────────────────────────
# We manually assign x positions so we can control bar spacing exactly:
# CC group centred at x=1: NS at 1-BAR_WIDTH/2-GAP/2, SR at 1+BAR_WIDTH/2+GAP/2
# FR group centred at x=2: NS at 2-...,  SR at 2+...
half  <- BAR_WIDTH / 2 + BAR_GAP / 2
pos   <- list(
  CC_NS = 1 - half,
  CC_SR = 1 + half,
  FR_NS = 2 - half,
  FR_SR = 2 + half
)

# ── 8. SIGNIFICANCE FUNCTION ──────────────────────────────────────────────────
sig_label <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("ns")
}

# ── 9. PLOT FACTORY ───────────────────────────────────────────────────────────
make_panel <- function(trait, ylabel, panel_tag) {

  # --- Summarise: mean ± SE for each LOC × TRT cell
  smry <- data %>%
    group_by(LOC, TRT) %>%
    summarise(
      mn = mean(.data[[trait]], na.rm = TRUE),
      se = sd(.data[[trait]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[trait]]))),
      .groups = "drop"
    ) %>%
    mutate(
      x = case_when(
        LOC == "CC" & TRT == "NS" ~ pos$CC_NS,
        LOC == "CC" & TRT == "SR" ~ pos$CC_SR,
        LOC == "FR" & TRT == "NS" ~ pos$FR_NS,
        LOC == "FR" & TRT == "SR" ~ pos$FR_SR
      ),
      fill_col = if_else(TRT == "NS", COL_NS, COL_SR)
    )

  # --- Paired t-test NS vs SR within CC, within FR
  wide <- data %>%
    select(GEN, LOC, TRT, all_of(trait)) %>%
    group_by(GEN, LOC, TRT) %>%
    summarise(v = mean(.data[[trait]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = TRT, values_from = v) %>%
    drop_na()

  # significance per location
  sig_cc <- tryCatch({
    d <- wide %>% filter(LOC == "CC")
    if (nrow(d) > 2) sig_label(t.test(d$NS, d$SR, paired = TRUE)$p.value) else "ns"
  }, error = function(e) "ns")

  sig_fr <- tryCatch({
    d <- wide %>% filter(LOC == "FR")
    if (nrow(d) > 2) sig_label(t.test(d$NS, d$SR, paired = TRUE)$p.value) else "ns"
  }, error = function(e) "ns")

  # --- Y-axis headroom for brackets
  y_max_cc <- smry %>% filter(LOC == "CC") %>% summarise(y = max(mn + se)) %>% pull(y)
  y_max_fr <- smry %>% filter(LOC == "FR") %>% summarise(y = max(mn + se)) %>% pull(y)
  y_global  <- max(smry$mn + smry$se, na.rm = TRUE)

  # bracket positions (per group)
  br_h_cc   <- y_max_cc * 1.10
  br_star_cc <- y_max_cc * 1.17
  br_h_fr   <- y_max_fr * 1.10
  br_star_fr <- y_max_fr * 1.17

  # overall y limit
  y_limit <- y_global * 1.35

  # ── build plot ────────────────────────────────────────────────────────────
  p <- ggplot() +

    # ── BARS (manual x positions, identity stat) ──
    geom_col(
      data = smry,
      aes(x = x, y = mn, fill = TRT),
      width = BAR_WIDTH,
      color = "black",
      linewidth = 0.3
    ) +

    # ── ERROR BARS ──
    geom_errorbar(
      data = smry,
      aes(x = x, ymin = mn - se, ymax = mn + se),
      width = ERROR_W,
      linewidth = ERROR_LWD,
      color = "black"
    ) +

    # ── DASHED VERTICAL DIVIDER between CC and FR ──
    geom_vline(
      xintercept = (pos$CC_SR + pos$FR_NS) / 2,
      linetype = "dashed",
      color = "grey50",
      linewidth = 0.35
    ) +

    # ── SIGNIFICANCE BRACKET: CC ──
    annotate("segment",
             x = pos$CC_NS, xend = pos$CC_SR,
             y = br_h_cc, yend = br_h_cc,
             linewidth = BRACKET_LWD) +
    annotate("segment",
             x = pos$CC_NS, xend = pos$CC_NS,
             y = br_h_cc * 0.975, yend = br_h_cc,
             linewidth = BRACKET_LWD) +
    annotate("segment",
             x = pos$CC_SR, xend = pos$CC_SR,
             y = br_h_cc * 0.975, yend = br_h_cc,
             linewidth = BRACKET_LWD) +
    annotate("text",
             x = (pos$CC_NS + pos$CC_SR) / 2,
             y = br_star_cc,
             label = sig_cc,
             size = 3.0,
             fontface = "bold",
             family = "Times New Roman") +

    # ── SIGNIFICANCE BRACKET: FR ──
    annotate("segment",
             x = pos$FR_NS, xend = pos$FR_SR,
             y = br_h_fr, yend = br_h_fr,
             linewidth = BRACKET_LWD) +
    annotate("segment",
             x = pos$FR_NS, xend = pos$FR_NS,
             y = br_h_fr * 0.975, yend = br_h_fr,
             linewidth = BRACKET_LWD) +
    annotate("segment",
             x = pos$FR_SR, xend = pos$FR_SR,
             y = br_h_fr * 0.975, yend = br_h_fr,
             linewidth = BRACKET_LWD) +
    annotate("text",
             x = (pos$FR_NS + pos$FR_SR) / 2,
             y = br_star_fr,
             label = sig_fr,
             size = 3.0,
             fontface = "bold",
             family = "Times New Roman") +

    # ── PANEL TAG top-left bold in parentheses ──
    annotate("text",
             x = -Inf, y = Inf,
             label = panel_tag,
             hjust = -0.15, vjust = 1.6,
             size = 3.5,
             fontface = "bold",
             family = "Times New Roman") +

    # ── COLORS: teal = NS, orange = SR ──
    scale_fill_manual(
      values = c("NS" = COL_NS, "SR" = COL_SR),
      name   = NULL
    ) +

    # ── X AXIS: group labels CC and FR at group centres ──
    scale_x_continuous(
      breaks = c(1, 2),
      labels = c("CC", "FR"),
      limits = c(0.4, 2.6)
    ) +

    # ── Y AXIS: start at 0, add headroom ──
    scale_y_continuous(
      expand = expansion(mult = c(0, 0)),
      limits = c(0, y_limit)
    ) +

    labs(x = NULL, y = ylabel) +

    # ── LEGEND: top-right inside panel, small ──
    guides(
      fill = guide_legend(
        keywidth  = unit(0.5, "lines"),
        keyheight = unit(0.5, "lines"),
        label.theme = element_text(size = LEGEND_SIZE,
                                   family = "Times New Roman")
      )
    ) +

    theme_classic(base_family = "Times New Roman", base_size = BASE_SIZE) +
    theme(
      # axes
      axis.text.x  = element_text(size = AXIS_TEXT, color = "black",
                                   face = "plain"),
      axis.text.y  = element_text(size = AXIS_TEXT, color = "black"),
      axis.title.y = element_text(size = AXIS_TITLE, color = "black",
                                   face = "bold", margin = margin(r = 3)),
      axis.line    = element_line(color = "black", linewidth = 0.4),
      axis.ticks   = element_line(color = "black", linewidth = 0.3),
      axis.ticks.length = unit(0.12, "cm"),

      # panel
      panel.border     = element_blank(),
      panel.grid       = element_blank(),
      panel.background = element_blank(),

      # legend: inside panel, top-right
      legend.position        = c(0.97, 0.97),
      legend.justification   = c(1, 1),
      legend.background      = element_rect(fill = NA, color = NA),
      legend.key             = element_rect(fill = NA, color = NA),
      legend.key.size        = unit(0.45, "lines"),
      legend.text            = element_text(size = LEGEND_SIZE),
      legend.spacing.y       = unit(0.08, "cm"),
      legend.margin          = margin(0, 0, 0, 0),

      # plot margins (tight)
      plot.margin = margin(t = 6, r = 4, b = 4, l = 4, unit = "pt")
    )

  return(p)
}

# ── 10. GENERATE ALL 12 PANELS ────────────────────────────────────────────────
plot_list <- mapply(
  FUN      = make_panel,
  trait    = traits,
  ylabel   = trait_labels,
  panel_tag = panel_tags,
  SIMPLIFY = FALSE
)

# ── 11. ASSEMBLE — 4 columns × 3 rows ────────────────────────────────────────
final_plot <- wrap_plots(plot_list, ncol = 4, nrow = 3) &
  theme(plot.margin = margin(4, 4, 4, 4, unit = "pt"))

# ── 12. EXPORT ────────────────────────────────────────────────────────────────
ggsave(
  filename = "trait_analysis_FINAL.tiff",
  plot     = final_plot,
  width    = 18,        # cm — adjust to journal spec
  height   = 14,
  units    = "cm",
  dpi      = 600,
  device   = "tiff",
  compression = "lzw"  # smaller file size
)

message("✔  Saved: trait_analysis_FINAL.tiff")

# ── Optional: quick PDF preview ───────────────────────────────────────────────
# ggsave("trait_analysis_preview.pdf", final_plot,
#         width = 18, height = 14, units = "cm")
