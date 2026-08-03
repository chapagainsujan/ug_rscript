library(tidyverse)
library(ggplot2)
library(extrafont)
library(grid)

loadfonts()

# Sample data transformation (replace NDVI with your data)
ndvi_long <- NDVI %>%
  pivot_longer(cols = -c(GEN, REP, TRT),
               names_to = "Stage", values_to = "NDVI")

stage_order <- c("Tillering", "Jointing", "Booting", "Heading", "Anthesis", "Soft Dough")
ndvi_long$Stage <- factor(ndvi_long$Stage, levels = stage_order)

ndvi_summary <- ndvi_long %>%
  group_by(TRT, Stage) %>%
  summarise(Mean_NDVI = mean(NDVI, na.rm = TRUE),
            SD_NDVI = sd(NDVI, na.rm = TRUE),
            .groups = "drop")

shape_values <- c("IRRIGATED" = 17, "DROUGHT" = 16)
color_values <- c("IRRIGATED" = "#000000", "DROUGHT" = "#7f7f7f")

# Create grobs for left and bottom lines as annotation_custom objects

left_line <- annotation_custom(
  grob = linesGrob(x = unit(c(0, 0), "npc"),
                   y = unit(c(0, 1), "npc"),
                   gp = gpar(col = "black", lwd = 1))
)

bottom_line <- annotation_custom(
  grob = linesGrob(x = unit(c(0, 1), "npc"),
                   y = unit(c(0, 0), "npc"),
                   gp = gpar(col = "black", lwd = 1))
)

p <- ggplot(ndvi_summary, aes(Stage, Mean_NDVI, group = TRT, color = TRT, shape = TRT)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_NDVI - SD_NDVI, ymax = Mean_NDVI + SD_NDVI),
                width = 0.15, size = 0.5) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = color_values) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman", size = 10),
    axis.title = element_text(color = "black"),
    axis.text = element_text(size = 11, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 7, family = "Times New Roman"),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    panel.border = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2)) +
  labs(x = "",
       y = "NDVI") +
  left_line + bottom_line

print(p)

# Save to TIFF including half rectangle lines
tiff("NDVI_Treatment_LineGraph.tiff", width = 8, height = 5, units = "in", res = 300)
print(p)
dev.off()





library(patchwork)

# Add labels (A. and B.) to each plot
p_labeled <- p + 
  labs(tag = "A.") + 
  theme(
    plot.tag = element_text(size = 16, face = "bold"),
    plot.tag.position = c(0.01, 0.85)  # Adjust position (x, y)
  )

p2_labeled <- p2 + 
  labs(tag = "B.") + 
  theme(
    plot.tag = element_text(size = 16, face = "bold"),
    plot.tag.position = c(0.01, 0.85)  # Adjust position (x, y)
  )

# Combine plots side by side with a gap
combined_plot <- p_labeled + p2_labeled + 
  plot_layout(nrow = 2) & 
  theme(plot.margin = margin(5, 5, 5, 5, "pt"))  # Adjust margins

# Save the combined plot
tiff("Combined_NDVI111_CanopyTemp.tiff", width = 12, height = 13, units = "in", res = 300)
print(combined_plot)
dev.off()





library(patchwork)

# Label plots A and B
p_labeled <- p + 
  labs(tag = "A.") + 
  theme(
    plot.tag = element_text(size = 16, face = "bold"),
    plot.tag.position = c(0.01, 0.85)
  )

p2_labeled <- p2 + 
  labs(tag = "B.") + 
  theme(
    plot.tag = element_text(size = 16, face = "bold"),
    plot.tag.position = c(0.01, 0.85)
  )

# Add space using plot_spacer
combined_plot <- p_labeled / plot_spacer() / p2_labeled +
  plot_layout(heights = c(1, 0.1, 1)) & 
  theme(plot.margin = margin(5, 5, 5, 5, "pt"))

# Save the combined plot
tiff("Combined_NDVI111_CanopyTemp.tiff", width = 12, height = 13, units = "in", res = 300)
print(combined_plot)
dev.off()
