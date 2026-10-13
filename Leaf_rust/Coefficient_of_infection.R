# Load required libraries
library(ggplot2)
library(reshape2)

# Create the data
data

# Convert genotype to factor to maintain order
data$Genotype <- factor(data$Genotype, levels = data$Genotype)

# Reshape data for plotting
data_long <- melt(data, id.vars = "Genotype", 
                  variable.name = "CI_Type", 
                  value.name = "Value")

# Create the plot
p <- ggplot(data_long, aes(x = Genotype, y = Value, color = CI_Type, group = CI_Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("CI1" = "#ef4444", "CI2" = "#3b82f6", "CI3" = "#22c55e"),
                     labels = c("CI1", "CI2", "CI3")) +
  labs(title = "Coefficient of Infection (CI)",
       x = "Genotype",
       y = "Coefficient of Infection",
       color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "top",
        legend.text = element_text(size = 11),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80))

# Display the plot
print(p)

# Save the plot
ggsave("coefficient_of_infection_chart.png", plot = p, 
       width = 12, height = 6, dpi = 300, bg = "white")
