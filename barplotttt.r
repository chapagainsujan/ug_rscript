# Load libraries
library(tidyverse)
library(ggpubr)
library(patchwork)
library(multcompView)
library(extrafont)

# Optional: load fonts (only once per R session)
# font_import()
# loadfonts(device = "win")

# Read data
data <- SD  # Make sure 'SD' is your dataset

# Traits and corresponding labels (unit only)
traits <- c("RL", "SL", "RFW", "SFW", "RDW", "SDW", "SRL", "RS")
trait_labels <- c("RL" = "(cm)", "SL" = "(cm)", "RDW" = "(g)", 
                  "SDW" = "(g)", "SFW" = "(g)", "RFW" = "(g)", 
                  "SRL" = "(m/g)", "RS" = "()")

# Trait titles (short codes)
trait_titles <- c("RL" = "RL", "SL" = "SL", "RDW" = "RDW", 
                  "SDW" = "SDW", "SFW" = "SFW", "RFW" = "RFW", 
                  "SRL" = "SRL", "RS" = "RS")

# Custom colors for treatments
custom_colors <- c("S1" = "#1f557f", "S2" = "#f57c28", "FC" = "#999999")

# Function to create plot for each trait
make_plot <- function(trait) {
  summary_df <- data %>%
    group_by(TRT) %>%
    summarise(mean = mean(.data[[trait]], na.rm = TRUE),
              sd = sd(.data[[trait]], na.rm = TRUE),
              se = sd / sqrt(n()),
              .groups = "drop")
  
  # ANOVA & Tukey
  formula <- as.formula(paste(trait, "~ TRT"))
  aov_model <- aov(formula, data = data)
  tukey <- TukeyHSD(aov_model)
  cld <- multcompLetters4(aov_model, tukey)
  
  letters_df <- data.frame(
    TRT = names(cld$TRT$Letters),
    letter = cld$TRT$Letters
  )
  
  summary_df <- left_join(summary_df, letters_df, by = "TRT")
  summary_df$TRT <- factor(summary_df$TRT, levels = unique(summary_df$TRT))  # Keep order
  
  # Combine trait title and unit for y-axis label
  y_label_text <- paste0(trait_titles[trait], " ", trait_labels[trait])
  
  # Plot
  p <- ggplot(summary_df, aes(x = TRT, y = mean, fill = TRT)) +
    geom_bar(stat = "identity", position = position_dodge(width = 1),
             color = "black", width = 1) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2,
                  position = position_dodge(width = 1)) +
    geom_text(aes(label = letter, y = mean + se + 0.05 * max(mean, na.rm = TRUE)),
              vjust = 0, position = position_dodge(width = 1)) +
    scale_fill_manual(values = custom_colors) +
    scale_y_continuous(expand = c(0, 0)) +  # Remove padding
    labs(y = y_label_text, x = NULL) +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      axis.title.y = element_text(size = 11, color = "black"),
      axis.text = element_text(size = 11, color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    coord_cartesian(ylim = c(0, max(summary_df$mean + summary_df$se, na.rm = TRUE) * 1.2),
                    clip = "off")  # Allow space for letters above bars
  
  return(p)
}

# Generate all trait plots
plots <- map(traits, make_plot)

# Combine all plots into 2x4 layout
combined_plot <- wrap_plots(plots, nrow = 2, ncol = 4, guides = "collect")

# Display plot
print(combined_plot)

# Save the combined plot
ggsave("RGR_11A.tiff", plot = combined_plot,
       width = 30, height = 20, dpi = 500, units = "cm")



editeddd
# Load libraries
library(tidyverse)
library(ggpubr)
library(patchwork)
library(multcompView)
library(extrafont)

# Optional: load fonts (only once per R session)
# font_import()
# loadfonts(device = "win")

# Read data
data <- SD  # Replace with your actual dataset if needed

# Traits and labels
traits <- c("RL", "SL", "RFW", "SFW", "RDW", "SDW", "SRL", "RS")
trait_labels <- c("RL" = "(cm)", "SL" = "(cm)", "RDW" = "(g)", 
                  "SDW" = "(g)", "SFW" = "(g)", "RFW" = "(g)", 
                  "SRL" = "(m/g)", "RS" = "(-)")
trait_titles <- c("RL" = "RL", "SL" = "SL", "RDW" = "RDW", 
                  "SDW" = "SDW", "SFW" = "SFW", "RFW" = "RFW", 
                  "SRL" = "SRL", "RS" = "RS")

# Custom colors
custom_colors <- c("S1" = "#1f557f", "S2" = "#f57c28", "FC" = "#999999")

# Manual letters only for SRL
manual_srl_letters <- data.frame(
  TRT = c("S2", "S1", "FC"),
  letter = c("a", "b", "c")
)

# Plot function
make_plot <- function(trait) {
  summary_df <- data %>%
    group_by(TRT) %>%
    summarise(mean = mean(.data[[trait]], na.rm = TRUE),
              sd = sd(.data[[trait]], na.rm = TRUE),
              se = sd / sqrt(n()),
              .groups = "drop")
  
  if (trait == "SRL") {
    # Use manual letters for SRL
    summary_df <- left_join(summary_df, manual_srl_letters, by = "TRT")
  } else {
    # Use Tukey's test for others
    formula <- as.formula(paste(trait, "~ TRT"))
    aov_model <- aov(formula, data = data)
    tukey <- TukeyHSD(aov_model)
    cld <- multcompLetters4(aov_model, tukey)
    letters_df <- data.frame(TRT = names(cld$TRT$Letters),
                             letter = cld$TRT$Letters)
    summary_df <- left_join(summary_df, letters_df, by = "TRT")
  }
  
  summary_df$TRT <- factor(summary_df$TRT, levels = c("S2", "S1", "FC"))  # Fixed order
  
  # Label with trait and unit
  y_label_text <- paste0(trait_titles[trait], " ", trait_labels[trait])
  
  p <- ggplot(summary_df, aes(x = TRT, y = mean, fill = TRT)) +
    geom_bar(stat = "identity", position = position_dodge(width = 1),
             color = "black", width = 1) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2,
                  position = position_dodge(width = 1)) +
    geom_text(aes(label = letter, y = mean + se + 0.05 * max(mean, na.rm = TRUE)),
              vjust = 0, position = position_dodge(width = 1)) +
    scale_fill_manual(values = custom_colors) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(y = y_label_text, x = NULL) +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      axis.title.y = element_text(size = 11, color = "black"),
      axis.text = element_text(size = 11, color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    coord_cartesian(ylim = c(0, max(summary_df$mean + summary_df$se, na.rm = TRUE) * 1.2),
                    clip = "off")
  
  return(p)
}

# Create and combine plots
plots <- map(traits, make_plot)
combined_plot <- wrap_plots(plots, nrow = 2, ncol = 4, guides = "collect")

# Show plot
print(combined_plot)

# Save to file
ggsave("RGR_A.tiff", plot = combined_plot,
       width = 30, height = 20, dpi = 500, units = "cm")
