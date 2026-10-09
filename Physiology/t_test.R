# Load required packages
library(tidyverse)
library(readxl)

# Define the trait columns (excluding GEN, REP, and TRT)
trait_cols <- colnames(Physio)[!(colnames(Physio) %in% c("GEN", "REP", "TRT"))]

# Initialize an empty list to store t-test results
t_test_results <- list()

# Function to assign stars based on p-value
p_stars <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else if (p < 0.1) return(".")
  else return("ns")
}

# Loop through each trait and perform t-test between TRT groups
for (trait in trait_cols) {
  test_result <- t.test(Physio[[trait]] ~ Physio$TRT)
  t_test_results[[trait]] <- data.frame(
    Trait = trait,
    Mean_TRT1 = test_result$estimate[1],
    Mean_TRT2 = test_result$estimate[2],
    t_value = test_result$statistic,
    p_value = test_result$p.value,
    Significance = p_stars(test_result$p.value)
  )
}

# Combine all results into one data frame
final_ttest <- bind_rows(t_test_results)

# View the result
print(final_ttest)

# Optionally, write to Excel or CSV
write.csv(final_ttest, "t_test123_trait_by_TRT.csv", row.names = FALSE)
