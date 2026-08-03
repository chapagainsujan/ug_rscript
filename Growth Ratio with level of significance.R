# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggpubr)

# Load dataset (Replace 'Growth' with actual dataset name)
df <- Growth

# Perform t-tests for RGRRW1 between treatments
t_test_TRT1_TRT2 <- t.test(RGRRW1 ~ TRT, data = subset(df, TRT %in% c("SCI", "DS")))
t_test_TRT1_TRT3 <- t.test(RGRRW1 ~ TRT, data = subset(df, TRT %in% c("SCI", "CT")))
t_test_TRT2_TRT3 <- t.test(RGRRW1 ~ TRT, data = subset(df, TRT %in% c("CT", "DS")))

# Extract p-values
p_value_TRT1_TRT2 <- t_test_TRT1_TRT2$p.value
p_value_TRT1_TRT3 <- t_test_TRT1_TRT3$p.value
p_value_TRT2_TRT3 <- t_test_TRT2_TRT3$p.value

# Define significance levels
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

# Apply function to p-values
sig_TRT1_TRT2 <- get_significance_label(p_value_TRT1_TRT2)
sig_TRT1_TRT3 <- get_significance_label(p_value_TRT1_TRT3)
sig_TRT2_TRT3 <- get_significance_label(p_value_TRT2_TRT3)

# Create bar plot with significance labels
p <- ggplot(df, aes(x = TRT, y = RGRRW1, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "RGR RW 30_60 Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Define y-position for annotations
y_max <- max(df$RGRRW1, na.rm = TRUE)

# Add significance annotations
a<-p + 
  annotate("text", x = 1.5, y = y_max + 0.02, label = sig_TRT1_TRT2, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.015, yend = y_max + 0.015, colour = "black") +
  
  annotate("text", x = 2.5, y = y_max + 0.04, label = sig_TRT1_TRT3, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.035, yend = y_max + 0.035, colour = "black") +
  
  annotate("text", x = 2, y = y_max + 0.06, label = sig_TRT2_TRT3, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.055, yend = y_max + 0.055, colour = "black")

a




####2###
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggpubr)

# Load dataset (Replace 'Growth' with actual dataset name)
df <- Growth

# Perform t-tests for RGRRW1 between treatments
t_test_TRT1_TRT2 <- t.test(RGRRW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "DS")))
t_test_TRT1_TRT3 <- t.test(RGRRW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "CT")))
t_test_TRT2_TRT3 <- t.test(RGRRW2 ~ TRT, data = subset(df, TRT %in% c("CT", "DS")))

# Extract p-values
p_value_TRT1_TRT2 <- t_test_TRT1_TRT2$p.value
p_value_TRT1_TRT3 <- t_test_TRT1_TRT3$p.value
p_value_TRT2_TRT3 <- t_test_TRT2_TRT3$p.value

# Define significance levels
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

# Apply function to p-values
sig_TRT1_TRT2 <- get_significance_label(p_value_TRT1_TRT2)
sig_TRT1_TRT3 <- get_significance_label(p_value_TRT1_TRT3)
sig_TRT2_TRT3 <- get_significance_label(p_value_TRT2_TRT3)

# Create bar plot with significance labels
p <- ggplot(df, aes(x = TRT, y = RGRRW2, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "RGR RW 60_90 Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Define y-position for annotations
y_max <- max(df$RGRRW2, na.rm = TRUE)

# Add significance annotations
b<-p + 
  annotate("text", x = 1.5, y = y_max + 0.02, label = sig_TRT1_TRT2, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.015, yend = y_max + 0.015, colour = "black") +
  
  annotate("text", x = 2.5, y = y_max + 0.04, label = sig_TRT1_TRT3, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.035, yend = y_max + 0.035, colour = "black") +
  
  annotate("text", x = 2, y = y_max + 0.06, label = sig_TRT2_TRT3, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.055, yend = y_max + 0.055, colour = "black")

b




####3###
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggpubr)

# Load dataset (Replace 'Growth' with actual dataset name)
df <- Growth

# Perform t-tests for RGRRW1 between treatments
t_test_TRT1_TRT2 <- t.test(CGRRW1 ~ TRT, data = subset(df, TRT %in% c("SCI", "DS")))
t_test_TRT1_TRT3 <- t.test(CGRRW1 ~ TRT, data = subset(df, TRT %in% c("SCI", "CT")))
t_test_TRT2_TRT3 <- t.test(CGRRW1 ~ TRT, data = subset(df, TRT %in% c("CT", "DS")))

# Extract p-values
p_value_TRT1_TRT2 <- t_test_TRT1_TRT2$p.value
p_value_TRT1_TRT3 <- t_test_TRT1_TRT3$p.value
p_value_TRT2_TRT3 <- t_test_TRT2_TRT3$p.value

# Define significance levels
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

# Apply function to p-values
sig_TRT1_TRT2 <- get_significance_label(p_value_TRT1_TRT2)
sig_TRT1_TRT3 <- get_significance_label(p_value_TRT1_TRT3)
sig_TRT2_TRT3 <- get_significance_label(p_value_TRT2_TRT3)

# Create bar plot with significance labels
p <- ggplot(df, aes(x = TRT, y = CGRRW1, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "CGR RW 30_60 Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Define y-position for annotations
y_max <- max(df$CGRRW1, na.rm = TRUE)

# Add significance annotations
C<-p + 
  annotate("text", x = 1.5, y = y_max + 0.02, label = sig_TRT1_TRT2, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.015, yend = y_max + 0.015, colour = "black") +
  
  annotate("text", x = 2.5, y = y_max + 0.04, label = sig_TRT1_TRT3, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.035, yend = y_max + 0.035, colour = "black") +
  
  annotate("text", x = 2, y = y_max + 0.06, label = sig_TRT2_TRT3, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.055, yend = y_max + 0.055, colour = "black")

C


####3####

# Perform t-tests for RGRRW1 between treatments
t_test_TRT1_TRT2 <- t.test(CGRRW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "DS")))
t_test_TRT1_TRT3 <- t.test(CGRRW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "CT")))
t_test_TRT2_TRT3 <- t.test(CGRRW2 ~ TRT, data = subset(df, TRT %in% c("CT", "DS")))

# Extract p-values
p_value_TRT1_TRT2 <- t_test_TRT1_TRT2$p.value
p_value_TRT1_TRT3 <- t_test_TRT1_TRT3$p.value
p_value_TRT2_TRT3 <- t_test_TRT2_TRT3$p.value

# Define significance levels
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

# Apply function to p-values
sig_TRT1_TRT2 <- get_significance_label(p_value_TRT1_TRT2)
sig_TRT1_TRT3 <- get_significance_label(p_value_TRT1_TRT3)
sig_TRT2_TRT3 <- get_significance_label(p_value_TRT2_TRT3)

# Create bar plot with significance labels
p <- ggplot(df, aes(x = TRT, y = CGRRW2, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "CGR RW 60_90 Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Define y-position for annotations
y_max <- max(df$CGRRW2, na.rm = TRUE)

# Add significance annotations
D<-p + 
  annotate("text", x = 1.5, y = y_max + 0.02, label = sig_TRT1_TRT2, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.015, yend = y_max + 0.015, colour = "black") +
  
  annotate("text", x = 2.5, y = y_max + 0.04, label = sig_TRT1_TRT3, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.035, yend = y_max + 0.035, colour = "black") +
  
  annotate("text", x = 2, y = y_max + 0.06, label = sig_TRT2_TRT3, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.055, yend = y_max + 0.055, colour = "black")

D


####E###

# Perform t-tests for RGRRW1 between treatments
t_test_TRT1_TRT2 <- t.test(RGRSW1 ~ TRT, data = subset(df, TRT %in% c("SCI", "DS")))
t_test_TRT1_TRT3 <- t.test(RGRSW1 ~ TRT, data = subset(df, TRT %in% c("SCI", "CT")))
t_test_TRT2_TRT3 <- t.test(RGRSW1 ~ TRT, data = subset(df, TRT %in% c("CT", "DS")))

# Extract p-values
p_value_TRT1_TRT2 <- t_test_TRT1_TRT2$p.value
p_value_TRT1_TRT3 <- t_test_TRT1_TRT3$p.value
p_value_TRT2_TRT3 <- t_test_TRT2_TRT3$p.value

# Define significance levels
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

# Apply function to p-values
sig_TRT1_TRT2 <- get_significance_label(p_value_TRT1_TRT2)
sig_TRT1_TRT3 <- get_significance_label(p_value_TRT1_TRT3)
sig_TRT2_TRT3 <- get_significance_label(p_value_TRT2_TRT3)

# Create bar plot with significance labels
p <- ggplot(df, aes(x = TRT, y = RGRSW1, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "RGR SW 30_60 Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Define y-position for annotations
y_max <- max(df$RGRSW1, na.rm = TRUE)

# Add significance annotations
F<-p + 
  annotate("text", x = 1.5, y = y_max + 0.02, label = sig_TRT1_TRT2, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.015, yend = y_max + 0.015, colour = "black") +
  
  annotate("text", x = 2.5, y = y_max + 0.04, label = sig_TRT1_TRT3, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.035, yend = y_max + 0.035, colour = "black") +
  
  annotate("text", x = 2, y = y_max + 0.06, label = sig_TRT2_TRT3, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.055, yend = y_max + 0.055, colour = "black")

F

t_test_TRT1_TRT2 <- t.test(RGRSW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "DS")))
t_test_TRT1_TRT3 <- t.test(RGRSW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "CT")))
t_test_TRT2_TRT3 <- t.test(RGRSW2 ~ TRT, data = subset(df, TRT %in% c("CT", "DS")))

# Extract p-values
p_value_TRT1_TRT2 <- t_test_TRT1_TRT2$p.value
p_value_TRT1_TRT3 <- t_test_TRT1_TRT3$p.value
p_value_TRT2_TRT3 <- t_test_TRT2_TRT3$p.value

# Define significance levels
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

# Apply function to p-values
sig_TRT1_TRT2 <- get_significance_label(p_value_TRT1_TRT2)
sig_TRT1_TRT3 <- get_significance_label(p_value_TRT1_TRT3)
sig_TRT2_TRT3 <- get_significance_label(p_value_TRT2_TRT3)

# Create bar plot with significance labels
p <- ggplot(df, aes(x = TRT, y = RGRSW2, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "RGR SW 60_90 Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Define y-position for annotations
y_max <- max(df$RGRSW1, na.rm = TRUE)

# Add significance annotations
G<-p + 
  annotate("text", x = 1.5, y = y_max + 0.02, label = sig_TRT1_TRT2, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.015, yend = y_max + 0.015, colour = "black") +
  
  annotate("text", x = 2.5, y = y_max + 0.04, label = sig_TRT1_TRT3, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.035, yend = y_max + 0.035, colour = "black") +
  
  annotate("text", x = 2, y = y_max + 0.06, label = sig_TRT2_TRT3, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.055, yend = y_max + 0.055, colour = "black")

G

####2##
t_test_TRT1_TRT2 <- t.test(RGRSW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "DS")))
t_test_TRT1_TRT3 <- t.test(RGRSW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "CT")))
t_test_TRT2_TRT3 <- t.test(RGRSW2 ~ TRT, data = subset(df, TRT %in% c("CT", "DS")))

# Extract p-values
p_value_TRT1_TRT2 <- t_test_TRT1_TRT2$p.value
p_value_TRT1_TRT3 <- t_test_TRT1_TRT3$p.value
p_value_TRT2_TRT3 <- t_test_TRT2_TRT3$p.value

# Define significance levels
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

# Apply function to p-values
sig_TRT1_TRT2 <- get_significance_label(p_value_TRT1_TRT2)
sig_TRT1_TRT3 <- get_significance_label(p_value_TRT1_TRT3)
sig_TRT2_TRT3 <- get_significance_label(p_value_TRT2_TRT3)

# Create bar plot with significance labels
p <- ggplot(df, aes(x = TRT, y = RGRSW1, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "SGR RW 60_90 Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Define y-position for annotations
y_max <- max(df$RGRSW2, na.rm = TRUE)

# Add significance annotations
G<-p + 
  annotate("text", x = 1.5, y = y_max + 0.02, label = sig_TRT1_TRT2, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.015, yend = y_max + 0.015, colour = "black") +
  
  annotate("text", x = 2.5, y = y_max + 0.04, label = sig_TRT1_TRT3, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.035, yend = y_max + 0.035, colour = "black") +
  
  annotate("text", x = 2, y = y_max + 0.06, label = sig_TRT2_TRT3, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.055, yend = y_max + 0.055, colour = "black")

G


####H####
t_test_TRT1_TRT2 <- t.test(CGRSW1 ~ TRT, data = subset(df, TRT %in% c("SCI", "DS")))
t_test_TRT1_TRT3 <- t.test(CGRSW1 ~ TRT, data = subset(df, TRT %in% c("SCI", "CT")))
t_test_TRT2_TRT3 <- t.test(CGRSW1 ~ TRT, data = subset(df, TRT %in% c("CT", "DS")))

# Extract p-values
p_value_TRT1_TRT2 <- t_test_TRT1_TRT2$p.value
p_value_TRT1_TRT3 <- t_test_TRT1_TRT3$p.value
p_value_TRT2_TRT3 <- t_test_TRT2_TRT3$p.value

# Define significance levels
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

# Apply function to p-values
sig_TRT1_TRT2 <- get_significance_label(p_value_TRT1_TRT2)
sig_TRT1_TRT3 <- get_significance_label(p_value_TRT1_TRT3)
sig_TRT2_TRT3 <- get_significance_label(p_value_TRT2_TRT3)

# Create bar plot with significance labels
p <- ggplot(df, aes(x = TRT, y = CGRSW1, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "CGR SW 30_60 Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Define y-position for annotations
y_max <- max(df$CGRSW1, na.rm = TRUE)

# Add significance annotations
H<-p + 
  annotate("text", x = 1.5, y = y_max + 0.02, label = sig_TRT1_TRT2, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.015, yend = y_max + 0.015, colour = "black") +
  
  annotate("text", x = 2.5, y = y_max + 0.04, label = sig_TRT1_TRT3, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.035, yend = y_max + 0.035, colour = "black") +
  
  annotate("text", x = 2, y = y_max + 0.06, label = sig_TRT2_TRT3, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.055, yend = y_max + 0.055, colour = "black")

H


t_test_TRT1_TRT2 <- t.test(CGRSW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "DS")))
t_test_TRT1_TRT3 <- t.test(CGRSW2 ~ TRT, data = subset(df, TRT %in% c("SCI", "CT")))
t_test_TRT2_TRT3 <- t.test(CGRSW2 ~ TRT, data = subset(df, TRT %in% c("CT", "DS")))

# Extract p-values
p_value_TRT1_TRT2 <- t_test_TRT1_TRT2$p.value
p_value_TRT1_TRT3 <- t_test_TRT1_TRT3$p.value
p_value_TRT2_TRT3 <- t_test_TRT2_TRT3$p.value

# Define significance levels
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

# Apply function to p-values
sig_TRT1_TRT2 <- get_significance_label(p_value_TRT1_TRT2)
sig_TRT1_TRT3 <- get_significance_label(p_value_TRT1_TRT3)
sig_TRT2_TRT3 <- get_significance_label(p_value_TRT2_TRT3)

# Create bar plot with significance labels
p <- ggplot(df, aes(x = TRT, y = CGRSW2, fill = TRT)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1) +
  scale_fill_manual(values = c("blue", "orange", "green")) +
  labs(x = "Treatment", y = "CGR SW 60_90 Value") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", family = "Times New Roman", size = 13), 
    axis.text = element_text(face = "bold", family = "Times New Roman", size = 11),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Define y-position for annotations
y_max <- max(df$CGRSW2, na.rm = TRUE)

# Add significance annotations
I<-p + 
  annotate("text", x = 1.5, y = y_max + 0.02, label = sig_TRT1_TRT2, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_max + 0.015, yend = y_max + 0.015, colour = "black") +
  
  annotate("text", x = 2.5, y = y_max + 0.04, label = sig_TRT1_TRT3, size = 5) +
  annotate("segment", x = 1, xend = 3, y = y_max + 0.035, yend = y_max + 0.035, colour = "black") +
  
  annotate("text", x = 2, y = y_max + 0.06, label = sig_TRT2_TRT3, size = 5) +
  annotate("segment", x = 2, xend = 3, y = y_max + 0.055, yend = y_max + 0.055, colour = "black")

I



aa<-a + b + C  +D+ G + F +H +I+  patchwork::plot_layout(nrow = 2, ncol = 4) 

ggsave(filename = "RGR AND CGR.png", plot = aa,width = 30, height = 20, dpi = 500, units = "cm")
