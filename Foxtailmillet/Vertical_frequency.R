# ============================================================
# FOXTAIL MILLET QUALITATIVE TRAITS
# 100% STACKED BAR CHART
#
# Layout:
#   Bars attached directly to 0% x-axis
#   Trait names below x-axis
#   No biological grouping titles
#
# Journal-style figure
# Light, restrained colours
# Black percentage labels
# Times New Roman
# 600 dpi
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(ggplot2)
library(dplyr)
library(grid)


# ============================================================
# 2. DATA
# ============================================================

data <- data.frame(
  
  Trait = c(
    
    # --------------------------------------------------------
    # LEAF TRAITS
    # --------------------------------------------------------
    
    "Leaf pedicel anthocyanin colour",
    "Leaf pedicel anthocyanin colour",
    "Leaf pedicel anthocyanin colour",
    
    "Leaf sheath anthocyanin",
    "Leaf sheath anthocyanin",
    
    "Leaf blade attitude",
    "Leaf blade attitude",
    "Leaf blade attitude",
    
    "Leaf colour",
    "Leaf colour",
    "Leaf colour",
    
    "Flag leaf anthocyanin",
    "Flag leaf anthocyanin",
    
    # --------------------------------------------------------
    # ROOT
    # --------------------------------------------------------
    
    "Brace root colour",
    "Brace root colour",
    
    # --------------------------------------------------------
    # PANICLE TRAITS
    # --------------------------------------------------------
    
    "Panicle shape",
    "Panicle shape",
    "Panicle shape",
    
    "Panicle attitude",
    "Panicle attitude",
    "Panicle attitude",
    
    "Bristle length",
    "Bristle length",
    "Bristle length",
    
    "Bristle anthocyanin colour",
    "Bristle anthocyanin colour",
    
    "Peduncle length",
    "Peduncle length",
    "Peduncle length",
    
    # --------------------------------------------------------
    # GRAIN
    # --------------------------------------------------------
    
    "Grain shape",
    "Grain shape",
    
    "Grain colour",
    "Grain colour"
  ),
  
  
  # ==========================================================
  # CHARACTER
  # ==========================================================
  
  Character = c(
    
    "Weak",
    "Medium",
    "Strong",
    
    "Weak",
    "Medium",
    
    "Erect",
    "Semi-erect",
    "Horizontal",
    
    "Light",
    "Medium",
    "Dark",
    
    "Weak",
    "Medium",
    
    "Absent",
    "Present",
    
    "Conical",
    "Spindle",
    "Cylinder",
    
    "Semi-erect",
    "Horizontal",
    "Moderately drooping",
    
    "Short",
    "Medium",
    "Long",
    
    "Absent",
    "Present",
    
    "Short",
    "Medium",
    "Long",
    
    "Medium ovate",
    "Circular",
    
    "Yellow",
    "Black"
  ),
  
  
  # ==========================================================
  # FREQUENCY
  # ==========================================================
  
  Frequency = c(
    
    82, 9, 9,
    
    73, 27,
    
    18, 27, 55,
    
    27, 27, 46,
    
    82, 18,
    
    82, 18,
    
    36, 9, 55,
    
    36, 18, 46,
    
    27, 64, 9,
    
    73, 27,
    
    18, 27, 55,
    
    73, 27,
    
    91, 9
  )
)


# ============================================================
# 3. TRAIT ORDER
# ============================================================

trait_order <- c(
  
  "Leaf pedicel anthocyanin colour",
  "Leaf sheath anthocyanin",
  "Leaf blade attitude",
  "Leaf colour",
  "Flag leaf anthocyanin",
  
  "Brace root colour",
  
  "Panicle shape",
  "Panicle attitude",
  "Bristle length",
  "Bristle anthocyanin colour",
  "Peduncle length",
  
  "Grain shape",
  "Grain colour"
)


data$Trait <- factor(
  data$Trait,
  levels = trait_order
)


# ============================================================
# 4. CHARACTER ORDER
# ============================================================

data$Character <- factor(
  data$Character,
  levels = unique(data$Character)
)


# ============================================================
# 5. PERCENTAGE LABEL
# ============================================================

data <- data %>%
  mutate(
    Label = paste0(
      Frequency,
      "%"
    )
  )


# ============================================================
# 6. COLOUR PALETTE
#
# Restrained journal-style colours
# Light enough for black percentage text
# ============================================================

my_colors <- c(
  
  # ----------------------------------------------------------
  # LEAF TRAITS
  # ----------------------------------------------------------
  
  "Weak"       = "#B7D7E8",
  "Medium"     = "#76B5D5",
  "Strong"     = "#4A90B8",
  
  "Erect"      = "#A8CFE3",
  "Semi-erect" = "#6FA8C4",
  "Horizontal" = "#397A9E",
  
  "Light"      = "#D2E6F0",
  "Dark"       = "#5F9FC0",
  
  
  # ----------------------------------------------------------
  # ROOT
  # ----------------------------------------------------------
  
  "Absent"     = "#B7D9B1",
  "Present"    = "#6EAA72",
  
  
  # ----------------------------------------------------------
  # PANICLE TRAITS
  # ----------------------------------------------------------
  
  "Conical"             = "#F6D6AD",
  "Spindle"             = "#E9A66C",
  "Cylinder"            = "#C87943",
  
  "Moderately drooping" = "#EDBC91",
  
  "Short"               = "#F2C1A0",
  "Long"                = "#D8895B",
  
  
  # ----------------------------------------------------------
  # GRAIN
  # ----------------------------------------------------------
  
  "Medium ovate" = "#D4C4E3",
  "Circular"    = "#A88BC0",
  
  "Yellow"      = "#C3B2D5",
  "Black"       = "#80649B"
)


# ============================================================
# 7. TRAIT LABELS
# ============================================================

trait_labels <- c(
  
  "Leaf pedicel\nanthocyanin colour",
  
  "Leaf sheath\nanthocyanin",
  
  "Leaf blade\nattitude",
  
  "Leaf\ncolour",
  
  "Flag leaf\nanthocyanin",
  
  "Brace root\ncolour",
  
  "Panicle\nshape",
  
  "Panicle\nattitude",
  
  "Bristle\nlength",
  
  "Bristle anthocyanin\ncolour",
  
  "Peduncle\nlength",
  
  "Grain\nshape",
  
  "Grain\ncolour"
)


# ============================================================
# 8. CREATE PLOT
# ============================================================

p <- ggplot(
  data,
  aes(
    x = Trait,
    y = Frequency,
    fill = Character
  )
) +
  
  
  # ==========================================================
# STACKED BARS
#
# Bars remain attached exactly to the 0% x-axis.
# ==========================================================

geom_col(
  width = 0.72,
  colour = "white",
  linewidth = 0.9
) +
  
  
  # ==========================================================
# PERCENTAGE LABELS
# ==========================================================

geom_text(
  aes(
    label = Label
  ),
  position = position_stack(
    vjust = 0.5
  ),
  size = 4.0,
  family = "Times New Roman",
  fontface = "bold",
  colour = "black"
) +
  
  
  # ==========================================================
# COLOUR SCALE
# ==========================================================

scale_fill_manual(
  values = my_colors
) +
  
  
  # ==========================================================
# Y AXIS
#
# Slight expansion is added ONLY outside the plotting
# range through coord_cartesian below.
# ==========================================================

scale_y_continuous(
  
  limits = c(
    0,
    100
  ),
  
  breaks = seq(
    0,
    100,
    20
  ),
  
  expand = c(
    0,
    0
  ),
  
  labels = function(x) {
    paste0(
      x,
      "%"
    )
  }
) +
  
  
  # ==========================================================
# X AXIS
#
# Trait names remain below the x-axis.
# ==========================================================

scale_x_discrete(
  
  labels = trait_labels,
  
  expand = expansion(
    add = c(
      0.35,
      0.35
    )
  )
) +
  
  
  # ==========================================================
# AXIS LABELS
# ==========================================================

labs(
  
  x = NULL,
  
  y = "Frequency (%)",
  
  fill = "Character"
) +
  
  
  # ==========================================================
# JOURNAL STYLE
# ==========================================================

theme_classic(
  base_family = "Times New Roman"
) +
  
  
  theme(
    
    # --------------------------------------------------------
    # X-AXIS TRAIT LABELS
    # --------------------------------------------------------
    
    axis.text.x = element_text(
      
      size = 14,
      
      colour = "black",
      
      angle = 90,
      
      hjust = 1,
      
      vjust = 1,
      
      margin = margin(
        t = 9
      )
    ),
    
    
    # --------------------------------------------------------
    # X-AXIS TICKS
    # --------------------------------------------------------
    
    axis.ticks.x = element_line(
      
      colour = "black",
      
      linewidth = 0.6
    ),
    
    
    # --------------------------------------------------------
    # Y-AXIS NUMBERS
    # --------------------------------------------------------
    
    axis.text.y = element_text(
      
      size = 12,
      
      colour = "black"
    ),
    
    
    # --------------------------------------------------------
    # Y-AXIS TITLE
    # --------------------------------------------------------
    
    axis.title.y = element_text(
      
      size = 15,
      
      face = "bold",
      
      colour = "black"
    ),
    
    
    # --------------------------------------------------------
    # LEGEND TITLE
    # --------------------------------------------------------
    
    legend.title = element_text(
      
      size = 13,
      
      face = "bold",
      
      colour = "black"
    ),
    
    
    # --------------------------------------------------------
    # LEGEND TEXT
    # --------------------------------------------------------
    
    legend.text = element_text(
      
      size = 10,
      
      colour = "black"
    ),
    
    
    # --------------------------------------------------------
    # LEGEND KEY HEIGHT
    # --------------------------------------------------------
    
    legend.key.height = unit(
      
      0.55,
      
      "cm"
    ),
    
    
    # --------------------------------------------------------
    # LEGEND KEY WIDTH
    # --------------------------------------------------------
    
    legend.key.width = unit(
      
      0.55,
      
      "cm"
    ),
    
    
    # --------------------------------------------------------
    # LEGEND POSITION
    # --------------------------------------------------------
    
    legend.position = "right",
    
    
    # --------------------------------------------------------
    # PLOT MARGINS
    #
    # Slightly more space around the figure.
    # --------------------------------------------------------
    
    plot.margin = margin(
      
      top = 18,
      
      right = 25,
      
      bottom = 30,
      
      left = 23
    )
  ) +
  
  
  # ==========================================================
# COORDINATE SYSTEM
#
# Bars remain physically attached to 0%.
#
# A small visual gap is created outside the 0–100%
# data range so the figure does not look cramped.
# ==========================================================

coord_cartesian(
  
  ylim = c(
    0,
    103
  ),
  
  clip = "on"
)


# ============================================================
# 9. DISPLAY
# ============================================================

print(p)


# ============================================================
# 10. SAVE AS 600 DPI PNG
#
# Slightly wider than the previous version.
# ============================================================

ggsave(
  
  filename = "Foxtail_millet_qualitative22.png",
  
  plot = p,
  
  width = 11.5,
  
  height = 7,
  
  units = "in",
  
  dpi = 600,
  
  bg = "white"
)
