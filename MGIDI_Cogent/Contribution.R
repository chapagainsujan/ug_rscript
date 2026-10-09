library(metan)
library(ggplot2)
library(dplyr)
library(tidyr)

# Fit the model
model = gamem(Millet,
              gen = Landraces,
              rep = REP,
              resp = everything())

# BLUPs for genotypes
a = gmd(model, "blupg")

# Compute the MGIDI index
aku = mgidi(model)
gmd(aku, "MGIDI")

# Plot the contribution of each factor on the MGIDI index
p1 = plot(aku, type = "contribution") +
  labs(x = "", y = "Contribution to MGIDI") +  # Change axis labels
  theme(text = element_text(family = "Times New Roman", size = 10))

# Display plot
p1

p2 <- plot(aku) +
  labs(x = "", y = "MGIDI Index") +  
  theme(
    text = element_text(family = "Times New Roman", size = 15),
    axis.text.x = element_text(size = 14, family = "Times New Roman", face = "bold") # Adjust landrace name size
  )

# Display plot
p2

ggsave(filename = "11.png", plot = p1, width = 17, height = 14, dpi = 400, units = "cm", bg = "white")
ggsave(filename = "22.png", plot = p2, width = 27, height = 25, dpi = 400, units = "cm", bg = "white")
