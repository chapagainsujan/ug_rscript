
# Generate the plot
b <- plot(gge_model,
          type = 8,
          col.gen = "black",
          col.env = "brown",
          size.text.gen = 6,size.text.env = 7)

# Apply classic theme and Times New Roman font for clarity
p <- b + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    text = element_text(family = "Times New Roman", size = 14),
    axis.text = element_text(family = "Times New Roman", size = 12),
    axis.title = element_text(family = "Times New Roman", size = 14),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 13),
    plot.title = element_text(family = "Times New Roman", size = 15, hjust = 0.5)
  )

# Display the plot
print(p)

# Optionally save the plot
ggsave("RANKING_GENOTYPES.png", plot = p, width = 9, height = 9, dpi = 400)
