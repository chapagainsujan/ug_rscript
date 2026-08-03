# ================================
# 1. Libraries
# ================================
library(ggplot2)
library(dplyr)
library(extrafont)

loadfonts(device = "win")

# ================================
# 2. Data
# ================================
nepali <- c(6.47, 6.42)

international <- c(
  9.17,5.26,5.46,5.15,4.94,4.79,7.79,4.91,5.60,6.50,
  5.10,5.30,5.25,5.50,5.50,5.50,8.75,6.00,8.50,7.50,
  8.50,5.16,5.64,6.75,6.05,6.05,5.25,5.70,5.70,6.60,
  5.10,5.10,5.00,5.00,5.38,6.30,6.10,5.72,7.02,5.71,
  6.14,5.70,6.39,6.36,6.50,6.01,6.30
)

df <- data.frame(
  group = c(rep("Nepali Rice Hybrid", length(nepali)),
            rep("International Registered Rice Hybrid", length(international))),
  yield = c(nepali, international)
)

# ================================
# 3. Summary
# ================================
summary_df <- df %>%
  group_by(group) %>%
  summarise(
    mean = mean(yield),
    se = sd(yield) / sqrt(n())
  )

# ================================
# 4. t-test
# ================================
t_test <- t.test(nepali, international)

t_val <- round(as.numeric(t_test$statistic), 2)
p_val <- t_test$p.value

sig <- ifelse(p_val < 0.001, "***",
              ifelse(p_val < 0.01, "**",
                     ifelse(p_val < 0.05, "*", "ns")))

# ================================
# 5. SAFE SPACING SYSTEM (FIXED)
# ================================
y_max <- max(summary_df$mean + summary_df$se)

bracket_y <- y_max + 0.4
text_y <- y_max + 0.7
top_limit <- y_max + 1.2

# ================================
# 6. PLOT
# ================================
p <- ggplot(summary_df, aes(x = group, y = mean, fill = group)) +
  
  geom_col(width = 0.36, color = "black", linewidth = 0.3) +
  
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.06, size = 0.4
  ) +
  
  # bracket
  geom_segment(aes(x = 1, xend = 2,
                   y = bracket_y,
                   yend = bracket_y),
               linewidth = 0.3) +
  
  geom_segment(aes(x = 1, xend = 1,
                   y = bracket_y,
                   yend = bracket_y - 0.2),
               linewidth = 0.3) +
  
  geom_segment(aes(x = 2, xend = 2,
                   y = bracket_y,
                   yend = bracket_y - 0.2),
               linewidth = 0.3) +
  
  annotate("text",
           x = 1.5,
           y = text_y,
           label = paste0("italic(t)==", t_val, "*','~'", sig, "'"),
           parse = TRUE,
           family = "Times New Roman",
           size = 3) +
  
  scale_y_continuous(
    breaks = seq(0, 10, 1),
    limits = c(0, top_limit),
    expand = c(0, 0)
  ) +
  
  scale_x_discrete(expand = expansion(add = 0.6)) +
  
  scale_fill_manual(
    values = c("Nepali Rice Hybrid" = "#0072B2",
               "International Registered Rice Hybrid" = "#00BFC4"),
    name = NULL
  ) +
  
  labs(
    x = NULL,
    y = "Yield (t/ha)"
  ) +
  
  theme_bw(base_family = "Times New Roman") +
  
  theme(
    text = element_text(size = 8),
    axis.text = element_text(size = 7, color = "black"),
    axis.title = element_text(size = 8),
    
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(linewidth = 0.3),
    
    panel.border = element_rect(color = "black", linewidth = 0.35),
    panel.grid = element_blank(),
    
    plot.margin = margin(10, 15, 10, 15),
    legend.position = "none"
  ) +
  
  coord_cartesian(clip = "off")

# ================================
# 7. SAVE
# ================================
ggsave("Hybrid_Yield_Publication.png",
       plot = p,
       width = 4,
       height = 3.5,
       dpi = 600)