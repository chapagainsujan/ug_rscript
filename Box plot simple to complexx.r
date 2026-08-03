
#########SIMPLE SINGLE BOX PLOT########

## Load required libraries
library(ggplot2)
library(reshape2)

##### SELECT TRAITS (IMPORTANT SECTION) #####
traits <- c("FLA")   # <-- change traits here

##### CONVERT TO LONG FORMAT #####
FM_long <- melt(FM, measure.vars = traits)

## Create the plot
a <- ggplot(FM_long, aes(x = variable, y = value, color = variable)) +
  geom_boxplot() +                                  ## Add boxplot to show distribution
  geom_jitter(width = 0.1, alpha = 0.6) +          ## Add individual points with jitter
  theme_minimal() +                                 ## Apply minimal theme
  xlab("Traits") +                                  ## Label x-axis
  ylab("Value") +                                   ## Label y-axis
  theme(legend.position = "none") +                ## Remove legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  ## Rotate x-axis labels

## Save the plot as a high-resolution PNG
ggsave("aa.png", a, width = 4, height = 4, dpi = 400)






###################################3COMPLEX COMBINEDDDDDD#########################################################

library(ggplot2)
library(patchwork)
library(showtext)

# ── 2. Font Setup ────────────────────────────────────────────────
font_add("Times New Roman", regular = "C:/Windows/Fonts/times.ttf")
showtext_auto()

FM <- rawdata
data <- FM   # your dataset

# ── 3. Custom Theme ──────────────────────────────────────────────
base_size <- 16
title_size <- 14
tag_size <- 18

custom_theme <- theme(
  panel.grid.major = element_line(color = "gray80", size = 0.3),
  panel.grid.minor = element_line(color = "gray90", size = 0.2),
  panel.border     = element_blank(),
  axis.line        = element_line(color = "black"),
  axis.title.x     = element_text(size = base_size, family = "Times New Roman"),
  axis.title.y     = element_text(size = base_size, family = "Times New Roman"),
  axis.text.x      = element_blank(),
  axis.text.y      = element_text(size = base_size, color = "black", family = "Times New Roman"),
  plot.title       = element_text(size = title_size, hjust = 0.5, family = "Times New Roman")
)

# ── 4. Boxplot Function ──────────────────────────────────────────
make_boxplot <- function(data, trait, ylab_text) {
  
  mean_overall <- mean(data[[trait]], na.rm = TRUE)
  
  ggplot(data, aes(x = "", y = .data[[trait]])) +
    geom_boxplot(color = "#4C72B0", fill = "white", outlier.shape = NA, size = 0.6) +
    geom_jitter(width = 0.08, alpha = 0.5, size = 1.2, color = "#4C72B0") +
    annotate("point", x = 1, y = mean_overall, shape = 16, color = "black", size = 2) +
    ylab(ylab_text) + xlab(NULL) +
    ggtitle(trait) +
    theme_minimal() + custom_theme
}

# ── 5. Define Traits ─────────────────────────────────────────────
traits <- list(
  c("ET", "cm"),
  c("PH", "cm"),
  c("GY", "t/ha"),
  c("FLA", "days")
)

# ── 6. Generate Plots ───────────────────────────────────────────
plot_list <- lapply(traits, function(tr) {
  make_boxplot(FM, tr[1], tr[2])
})

# ── 7. USER CONTROL: Rows & Columns ─────────────────────────────
n_rows <- 2   # << CHANGE THIS
n_cols <- 2   # << CHANGE THIS

Final <- wrap_plots(plotlist = plot_list, nrow = n_rows, ncol = n_cols) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = tag_size, family = "Times New Roman", face = "bold")
  )

Final





###########################Final ENV##############################################################
make_boxplot <- function(data, trait, ylab_text) {
  
  ggplot(data, aes(x = ENV, y = .data[[trait]])) +
    
    geom_boxplot(color = "#E69F00", fill = "white",
                 outlier.shape = NA, size = 0.8) +
    
    geom_jitter(aes(color = ENV),
                width = 0.15, alpha = 0.6, size = 2) +
    
    stat_summary(fun = mean,
                 geom = "point",
                 shape = 18, size = 3, color = "black") +
    
    xlab("Environment") +
    ylab(ylab_text) +
    ggtitle(trait) +
    
    theme_grey() + custom_theme +
    
    theme(
      axis.text.x = element_text(size = base_size, angle = 45, hjust = 1),
      legend.position = "none"
    )
}
