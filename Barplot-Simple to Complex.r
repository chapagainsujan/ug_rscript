##### SIMPLE UNDERSTANDING ################

#-----------------------------
# LOAD LIBRARIES
#-----------------------------
library(dplyr)     ### for data manipulation (group, summarise)
library(ggplot2)   ### for plotting graphs
library(tidyr)     ### for pivot_longer (reshape data)

#-----------------------------
# LOAD DATA
#-----------------------------
aa <- rawdata      ### assign your dataset to a new name (aa)

#=============================
# STEP 1: CONVERT DATA TO LONG FORMAT
#=============================
data_long <- aa %>%  
  pivot_longer(
    cols = c(PH),              ### select trait (Plant Height)
    names_to = "Trait",        ### create column "Trait"
    values_to = "Value"        ### store values in "Value"
  )

### WHY? → ggplot works better with long format data

#=============================
# STEP 2: CALCULATE MEAN & SE
#=============================
summary_data <- data_long %>%
  group_by(GEN, Trait) %>%     ### group by Genotype and Trait
  summarise(
    mean = mean(Value),        ### calculate average
    se = sd(Value) / sqrt(n()),### calculate standard error
    .groups = "drop"
  ) %>%
  arrange(desc(mean))          ### sort from highest to lowest mean


#-----------------------------
# STEP 3: CREATE BAR PLOT
#-----------------------------
p1 <- ggplot(summary_data, aes(x = reorder(GEN, -mean), y = mean)) +

  geom_bar(
    stat = "identity",         ### use actual mean values
    fill = "#5DADE2",          ### bar color
    width = 0.7
  ) +

  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),  ### error bar range
    width = 0.2,
    linewidth = 0.6
  ) +

  labs(
    x = "Genotype",            ### x-axis label
    y = "Plant Height (cm)"    ### y-axis label
  ) +

  theme_minimal(base_size = 12) +

  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), ### rotate x labels
    panel.grid.minor = element_blank()                 ### remove minor grid
  )

#-----------------------------
# STEP 4: DISPLAY PLOT
#-----------------------------
p1   ### show plot

#-----------------------------
# STEP 5: SAVE FIGURE
#-----------------------------
ggsave(
  "barplot_gen.png",   ### file name
  plot = p1,           ### plot to save
  width = 6,           ### width in inches
  height = 4,          ### height in inches
  dpi = 600,           ### high quality
  bg = "white"         ### background color
)





##### SINGLE COMPLEX (SIMPLE EXPLANATION) ################

#-----------------------------
# LOAD LIBRARIES
#-----------------------------
library(dplyr)     ### data manipulation
library(ggplot2)   ### plotting
library(tidyr)     ### reshape data (pivot)
library(grid)      ### for arrow in axis

#-----------------------------
# USER SETTINGS (CUSTOMIZATION)
#-----------------------------
bar_color       <- "#58D68D"   ### bar color
error_color     <- "gray40"    ### error bar color
axis_color      <- "black"     ### axis line color

base_font       <- "Times New Roman" ### font style
axis_text_size  <- 16                  ### axis text size
axis_title_size <- 18                  ### axis title size

#-----------------------------
# DATA PREPARATION
#-----------------------------
aa <- rawdata   ### dataset rename

# Convert to long format
data_long <- aa %>%
  pivot_longer(
    cols      = c(PH),         ### trait selection
    names_to  = "Trait",       ### column name
    values_to = "Value"        ### values column
  )

# Calculate mean and SE
summary_data <- data_long %>%
  group_by(GEN, Trait) %>%     ### group by genotype
  summarise(
    mean = mean(Value, na.rm = TRUE),         ### average
    se   = sd(Value, na.rm = TRUE) / sqrt(n()), ### standard error
    .groups = "drop"
  ) %>%
  arrange(desc(mean)) %>%      ### sort high → low
  mutate(
    GEN   = factor(GEN, levels = GEN), ### keep order in plot
    Index = as.numeric(GEN)             ### numeric index for ticks
  )


#-----------------------------
# AXIS PARAMETERS
#-----------------------------
y_max      <- max(summary_data$mean + summary_data$se) * 1.1  
### top limit (extra space above bars)

y_tick_end <- -0.05 * y_max  
### small negative space below x-axis (for ticks)

n_gen      <- nrow(summary_data)  
### number of genotypes

#-----------------------------
# PLOT
#-----------------------------
p <- ggplot(summary_data, aes(x = GEN, y = mean)) +

  # Bars
  geom_col(
    fill  = bar_color,   ### apply selected color
    width = 0.7
  ) +

  # Error bars
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),  ### error range
    width     = 0.2,
    color     = error_color,
    linewidth = 0.6
  ) +

  # Small vertical ticks below each bar
  geom_segment(
    aes(x = Index, xend = Index, y = 0, yend = y_tick_end),
    inherit.aes = FALSE,
    color       = axis_color,
    linewidth   = 0.4
  )

 

  +

  # Y-axis with arrow
  annotate(
    "segment",
    x     = 0.4,
    xend  = 0.4,
    y     = 0,
    yend  = y_max,
    color = axis_color,
    linewidth = 0.8,
    arrow = arrow(length = unit(0.25, "cm"), type = "closed")
  )

  +

  # X-axis baseline
  annotate(
    "segment",
    x     = 0.4,
    xend  = n_gen + 0.6,
    y     = 0,
    yend  = 0,
    color = axis_color,
    linewidth = 0.8
  )

  ### custom axis line (instead of default ggplot)

  +

  # Labels
  labs(
    x = "Genotypes",
    y = "Plant Height (cm)"
  ) +

  # Axis scaling
  scale_y_continuous(
    limits = c(y_tick_end, y_max), ### include negative space
    expand = c(0, 0)
  ) +

  # Theme (appearance)
  theme_minimal(base_family = base_font) +
  theme(
    axis.text.x = element_text(
      angle = 90,   ### vertical labels
      vjust = 0.5,
      hjust = 1,
      size  = axis_text_size,
      color = "black"
    ),
    axis.text.y = element_text(
      size  = axis_text_size,
      color = "black"
    ),
    axis.title.y = element_text(
      size = axis_title_size
    ),
    panel.grid = element_blank(),     ### remove grid
    plot.margin = margin(5, 5, 5, 5)
  ) +

  coord_cartesian(clip = "off")  ### allow drawing outside plot

#-----------------------------
# SAVE OUTPUT
#-----------------------------
ggsave(
  "PH_barplot_publication111.png",  ### file name
  plot        = p,
  width       = 8,    ### width (inch)
  height      = 5,    ### height (inch)
  dpi         = 600,  ### high resolution
  units       = "in"
)





##### MULTIPLE TRAIT BARPLOTS (ADVANCED BUT SIMPLE EXPLANATION) #####

# =========================================================
# 1. LOAD LIBRARIES
# =========================================================
library(dplyr)       ### data manipulation (group, summarise)
library(ggplot2)     ### plotting graphs
library(tidyr)       ### reshape data (long format)
library(patchwork)   ### combine multiple plots

# =========================================================
# 2. USER SETTINGS (CUSTOMIZATION SECTION)
# =========================================================

# Colors for each trait
trait_colors <- c(
  PH  = "#5DADE2",  ### blue (Plant Height)
  FLA = "#58D68D",  ### green (Flag Leaf Area)
  CT  = "#F5B041",  ### orange (Canopy Temp)
  GY  = "#AF7AC5"   ### purple (Grain Yield)
)

### IMPORTANT:
### Named colors → automatically match trait name

# Font and text sizes
base_font <- "Times New Roman"   ### publication font
axis_text_size <- 14             ### tick label size
axis_title_size <- 16            ### axis title size

plot_tags <- c("A", "B", "C", "D")  
### labels for multi-panel plots (A, B, C, D)

# =========================================================
# 3. FUNCTION TO CREATE INDIVIDUAL BARPLOT
# =========================================================

### FUNCTION = reusable code
### Instead of writing same code 4 times → we use function

create_barplot <- function(
    data,             ### dataset
    trait_name,       ### which trait (PH, FLA, etc.)
    bar_color,        ### bar color
    x_label,          ### x-axis label
    y_label,          ### y-axis label
    axis_text_size,   ### text size
    axis_title_size   ### title size
) {
  
  #-----------------------------
  # 3a. CONVERT TO LONG FORMAT
  #-----------------------------
  data_long <- data %>%
    pivot_longer(
      cols = all_of(trait_name),  ### select trait dynamically
      names_to = "Trait",         ### column name
      values_to = "Value"         ### values
    )
  
  ### WHY?
  ### makes code flexible for any trait
  
  #-----------------------------
  # 3b. CALCULATE MEAN & SE
  #-----------------------------
  summary_data <- data_long %>%
    group_by(GEN, Trait) %>%     ### group by genotype
    summarise(
      mean = mean(Value, na.rm = TRUE),           ### average
      se   = sd(Value, na.rm = TRUE) / sqrt(n()), ### standard error
      .groups = "drop"
    ) %>%
    arrange(desc(mean)) %>%      ### sort highest first
    mutate(
      GEN = factor(GEN, levels = GEN), ### keep sorted order
      Index = as.numeric(GEN)          ### for custom ticks
    )
  
  ### NOTE:
  ### mean → central value
  ### se → variation (used in error bars)
  
  #-----------------------------
  # 3c. AXIS CALCULATION
  #-----------------------------
  y_max <- max(summary_data$mean + summary_data$se) * 1.1  
  ### top limit with extra space
  
  y_tick_offset <- -0.05 * y_max  
  ### small space below x-axis
  
  n_gen <- nrow(summary_data)  
  ### number of genotypes
  
  #-----------------------------
  # 3d. CREATE PLOT
  #-----------------------------
  p <- ggplot(summary_data, aes(x = GEN, y = mean)) +
    
    # Bars
    geom_col(
      fill = bar_color,   ### trait-specific color
      width = 0.7
    ) +
    
    # Error bars
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.2,
      color = "gray40",
      linewidth = 0.6
    ) +
    
    # Small ticks under each bar
    geom_segment(
      aes(x = Index, xend = Index, y = 0, yend = y_tick_offset),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.4
    )
    
    ### makes plot look more scientific
    
    +
    
    # Y-axis arrow
    annotate(
      "segment",
      x = 0.4, xend = 0.4,
      y = 0, yend = y_max,
      arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
      color = "black",
      linewidth = 0.8
    )
    
    ### shows direction (increase)
    
    +
    
    # X-axis line
    annotate(
      "segment",
      x = 0.4, xend = n_gen + 0.6,
      y = 0, yend = 0,
      color = "black",
      linewidth = 0.8
    )
    
    +
    
    # Labels
    labs(
      x = x_label,
      y = y_label
    ) +
    
    # Axis scaling
    scale_y_continuous(
      limits = c(y_tick_offset, y_max),
      expand = c(0, 0)
    ) +
    
    # Theme
    theme_minimal(base_family = base_font) +
    theme(
      axis.text.x = element_text(
        angle = 90, vjust = 0.5, hjust = 1,
        size = axis_text_size, color = "black"
      ),
      axis.text.y = element_text(
        size = axis_text_size, color = "black"
      ),
      axis.title.x = element_text(size = axis_title_size),
      axis.title.y = element_text(size = axis_title_size),
      panel.grid = element_blank(),   ### remove grid
      plot.margin = margin(5, 5, 5, 5)
    ) +
    
    coord_cartesian(clip = "off")  ### allow drawing outside
  
  return(p)
}

# =========================================================
# 4. CREATE PLOTS (USING FUNCTION)
# =========================================================

### Each line = one trait plot

p1 <- create_barplot(aa, "PH",  trait_colors["PH"],  
                     x_label = "Genotypes", y_label = "Plant Height", 
                     axis_text_size, axis_title_size)

p2 <- create_barplot(aa, "FLA", trait_colors["FLA"], 
                     x_label = "Genotypes", y_label = "Flag Leaf Area", 
                     axis_text_size, axis_title_size)

p3 <- create_barplot(aa, "CT",  trait_colors["CT"],  
                     x_label = "Genotypes", y_label = "Canopy Temperature", 
                     axis_text_size, axis_title_size)

p4 <- create_barplot(aa, "GY",  trait_colors["GY"],  
                     x_label = "Genotypes", y_label = "Grain Yield", 
                     axis_text_size, axis_title_size)

### ADVANTAGE:
### one function → multiple plots (clean + reusable)

# =========================================================
# 5. COMBINE PLOTS (2x2 GRID)
# =========================================================

combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(face = "bold", size = 18))

### RESULT:
### A   B
### C   D

# Adjust layout
combined <- combined + 
  plot_layout(
    guides = "collect", 
    widths = c(1, 1), 
    heights = c(1, 1), 
    byrow = TRUE
  )

# =========================================================
# 6. SAVE FINAL OUTPUT
# =========================================================
ggsave(
  "Trait_barplots_2x2.png",
  plot = combined,
  width = 12,
  height = 10,
  dpi = 600,
  units = "in"
)





####################################ENV-GEN####################################################3


#=============================
# SECOND CODE: BAR PLOT GROUPED BY ENV AND GEN
#=============================

# Convert data from wide to long format
data_long <- aa %>% 
  pivot_longer(
    cols = c(PH),                 ### Select trait(s) to analyze (here PH = Plant Height)
    names_to = "Trait",           ### New column storing trait name
    values_to = "Value"           ### New column storing actual values
  )

# Calculate summary statistics (mean and standard error)
summary_data <- data_long %>%
  group_by(ENV, GEN, Trait) %>%   ### Group data by Environment, Genotype, and Trait
  summarise(
    mean = mean(Value),           ### Calculate average value for each group
    se = sd(Value) / sqrt(n()),   ### Standard Error = SD / √n (variation measure)
    .groups = "drop"              ### Remove grouping after summarizing
  ) %>%
  arrange(ENV, desc(mean))        ### Sort data: by ENV, then highest mean first

#-----------------------------
# PLOT
#-----------------------------
p2 <- ggplot(summary_data, aes(x = GEN, y = mean, fill = ENV)) +
  
  geom_bar(
    stat = "identity",            ### Use actual mean values (not count)
    position = position_dodge(width = 0.7), ### Separate bars side-by-side for ENV
    width = 0.6                   ### Control bar thickness
  ) +
  
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se), ### Error bars (± standard error)
    width = 0.2,               ### Width of error bar lines
    linewidth = 0.6,           ### Thickness of error bar lines
    position = position_dodge(width = 0.7)   ### Align error bars with bars
  ) +
  
  labs(
    x = "Genotype",             ### X-axis label
    y = "Plant Height (cm)",    ### Y-axis label
    fill = "Environment"        ### Legend title for ENV
  ) +
  
  theme_minimal(base_size = 12) + ### Clean theme with base font size
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), ### Tilt x labels for readability
    panel.grid.minor = element_blank(),                ### Remove minor grid lines
    panel.grid.major.x = element_blank()               ### Remove vertical grid lines
  )

# Display plot
p2

#-----------------------------
# SAVE FIGURE
#-----------------------------
ggsave(
  "barplot_env_gen.png",   ### File name
  plot = p2,               ### Plot object to save
  width = 6,               ### Width in inches
  height = 4,              ### Height in inches
  dpi = 600,               ### High resolution (good for publication)
  bg = "white"             ### Background color
)