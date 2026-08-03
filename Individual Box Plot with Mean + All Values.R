# Calculate the overall mean of PH across all landraces
"meanph" <- mean(Millet$HI, na.rm = TRUE)

aa<-ggplot(Millet, aes(x = "", y = HI)) +  # No grouping on the x-axis
  geom_boxplot(color = "#FF7782", fill = "white", outlier.shape = NA, size = 0.8) +  # Thicker red outer boundary
  geom_point(data = aggregate(HI ~ Landraces, data = Millet, mean), 
             aes(x = "", y = HI), 
             shape = 18,   # Diamond shape for mean of each landrace
             color = "#FF7782", 
             size = 4) +  # Adding mean values as red diamond shapes
  geom_jitter(alpha = 0.5, size = 1.85, width = 0.2) +  # Optional for visualizing individual points
  geom_point(aes(x = "", y = meanph), 
             shape = 16,  # Circle shape for the overall mean
             color = "black", 
             size = 3.5) +  # Custom symbol for the overall mean with larger size
  xlab("Landraces") +
  ylab(" Harvest Index (HI)") +
  theme_bw() +  # Black and white theme
  theme_minimal() + theme(axis.title.x = element_text(size = 14),  # Adjusting the size of the x-axis label
                          axis.title.y = element_text(size = 14))  # Adjusting the size of the y-axis label

aa
ggsave(filename = "Harvest Index.png", plot = aa,width = 15, height = 13, dpi = 2500, units = "cm")
