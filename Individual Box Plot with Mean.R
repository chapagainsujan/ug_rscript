# Calculating the overall mean for 'TSW'
meanph <- mean(Millet$TSW, na.rm = TRUE)

# Aggregating the data by Landraces to calculate the mean of 'TSW'
aggregated_data <- aggregate(TSW ~ Landraces, data = Millet, FUN = mean)

# Plotting
aa <- ggplot(Millet, aes(x = "", y = TSW)) +  # No grouping on the x-axis
  geom_boxplot(color = "#FF3344", fill = "white", outlier.shape = NA, size = 1.2, lwd = 1.5) +  # Thicker red outer boundary
  
  geom_point(data = aggregated_data, 
             aes(x = "", y = TSW), 
             shape = 18,   # Diamond shape for mean of each landrace
             color = "#FF3344", 
             size = 5) +  # Increasing mean values size
  
  annotate("point", x = 1, y = meanph, 
           shape = 16,  # Circle shape for the overall mean
           color = "black", 
           size = 4.5) +  # Larger custom symbol for the overall mean
  xlab(" Thousand Seed Weight (TSW)") +
  ylab("Values") +  # Adjust label size here if necessary
  theme_gray() +  # Applying grey box type theme
  theme(
    axis.title.x = element_text(size = 12, face = "bold", family = "Times New Roman"),  # Times New Roman font for x-axis
    axis.title.y = element_text(size = 12, face = "bold", family = "Times New Roman"),  # Times New Roman font for y-axis
    axis.text.y = element_text(size = 12, face = "bold", color = "black", family = "Times New Roman"),  # Y-axis labels in Times New Roman
    axis.text.x = element_blank(),  # Hiding x-axis ticks for clarity
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "Times New Roman")  # Title in Times New Roman
  ) 

# Display the plot
aa

# Save the plot
ggsave(filename = "TSW_plot.png", plot = aa, width = 18, height = 15, dpi = 3000, units = "cm")
