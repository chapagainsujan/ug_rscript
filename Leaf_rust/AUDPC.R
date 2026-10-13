# --- 1. SETUP: Load Packages ---
# Install if not already installed
# install.packages(c("ggplot2", "agricolae"))

library(ggplot2)
library(agricolae)

# --- 2. DATA: Simulated Wheat Leaf Rust Data ---
data_rust <- data wheat

# Convert severity (%) to proportion for logistic fitting
epsilon <- 1e-3  # avoid exact 0/1 for logistic model
data_rust$y <- pmin(pmax(data_rust$severity / 100, epsilon), 1 - epsilon)

# --- 3. MODEL: Logistic Fit using nls() ---
fit_logistic <- nls(
  y ~ 1 / (1 + exp(-(a + r * time))),
  data = data_rust,
  start = list(a = -4.5, r = 0.1)
)

cat("\n--- Logistic Model Fit Summary (Wheat Leaf Rust) ---\n")
print(summary(fit_logistic))

# --- 4. CALCULATION: Area Under the Disease Progress Curve (AUDPC) ---
audpc_result <- audpc(data_rust$severity, data_rust$time)

cat("\n--- AUDPC Result ---\n")
cat("AUDPC (Severity-Days):", round(audpc_result, 2), "\n")

# --- 5. PLOTTING: Epidemic Progression Curve ---
fitted_data <- data.frame(
  time = data_rust$time,
  Severity = predict(fit_logistic) * 100  # convert back to %
)

plot_curve <- ggplot(data_rust, aes(x = time, y = severity)) +
  
  # Visualize the area under the curve to highlight total disease pressure
  geom_area(aes(y = severity), fill = "lightblue", alpha = 0.4) +
  
  # Plot the fitted logistic curve
  geom_line(data = fitted_data, aes(y = Severity), color = "darkgreen", linewidth = 1.2) +
  
  # Show observed data points
  geom_point(color = "darkred", size = 3) +
  
  # Labels and Title
  labs(
    title = "Wheat Leaf Rust Epidemic Progression Curve",
    subtitle = paste0(
      "Logistic Rate (r): ", round(coef(fit_logistic)["r"], 3), 
      " | AUDPC: ", round(audpc_result, 2), " Severity-Days"
    ),
    x = "Time (Days After First Assessment)",
    y = "Disease Severity (%)"
  ) +
  
  # Set scales and theme
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_x_continuous(breaks = data_rust$time) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold")
  )

# Display the plot
print(plot_curve)

# --- 6. SAVE PLOT AS PNG ---
# Saving as PNG allows sharing, publication, or embedding in reports.
ggsave(
  filename = "wheat_leaf_rust_epidemic_curve.png",  # file name
  plot = plot_curve,                               # plot object to save
  width = 10, height = 6,bg= "white",                          # size in inches
  dpi = 300                                       # resolution for quality printing
)
