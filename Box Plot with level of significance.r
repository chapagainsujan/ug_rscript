
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggpubr)

# Sample Data (Replace 'Wheat' with your actual dataset)
df <- Wheat

# Perform t-test between treatments for parameter 'RL'
t_test_FC_S1 <- t.test(RL ~ TRT, data = subset(df, TRT %in% c("FC", "S1")))
t_test_FC_S2 <- t.test(RL ~ TRT, data = subset(df, TRT %in% c("FC", "S2")))
t_test_S1_S2 <- t.test(RL ~ TRT, data = subset(df, TRT %in% c("S1", "S2")))

# Extract p-values
p_value_FC_S1 <- t_test_FC_S1$p.value
p_value_FC_S2 <- t_test_FC_S2$p.value
p_value_S1_S2 <- t_test_S1_S2$p.value# Define significance levels
get_significance_label <- function(p) {
  if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else {
    return("NS")
  }
}

# Create significance labels
sig_label_FC_S1 <- get_significance_label(p_value_FC_S1)
sig_label_FC_S2 <- get_significance_label(p_value_FC_S2)
sig_label_S1_S2 <- get_significance_label(p_value_S1_S2)

# Create the bar plot
p <- ggplot(df, aes(x = TRT, y = RL, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "Root length (cm)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Add significance annotations
p + 
  annotate("text", x = 1.5, y = max(df$RL) + 2, label = sig_label_FC_S1, size = 5) +
  annotate("segment", x = 1, xend = 2, y = max(df$RL) + 1, yend = max(df$RL) + 1, colour = "black") +
  annotate("text", x = 2.5, y = max(df$RL) + 4, label = sig_label_FC_S2, size = 5) +
  annotate("segment", x = 1, xend = 3, y = max(df$RL) + 3, yend = max(df$RL) + 3, colour = "black") +
  annotate("text", x = 2, y = max(df$RL) + 6, label = sig_label_S1_S2, size = 5) +
  annotate("segment", x = 2, xend = 3, y = max(df$RL) + 5, yend = max(df$RL) + 5, colour = "black")
