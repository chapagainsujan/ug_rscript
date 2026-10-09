# the purpose of this script is to analyze the diversity of
# qualitative data measurements for finger millet accessions with chi-squared test

# Upload packages
library(tidyverse)
library(readxl)
library(openxlsx)

# Upload data (this file path is relative to my computer, change for your file location)

d = Chi_Square
## First, traits with a single characteristics cannot be assessed with
# chi-squared. The code below removes single-characteristic traits.

d1 = d %>%
  group_by(trait) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  select(-n, -prop)

# This code below conducts the chi-squared test for a single trait. 
# Notice, there are certain traits where a warning "Chi-squared approximations may be incorrect". 
# These could be addressed by adding "simulate.p.value" argument in chisq.test(), or left alone.
# This would be a form of bootstrapping. Here is some additional info (https://stats.stackexchange.com/questions/81483/warning-in-r-chi-squared-approximation-may-be-incorrect)

blast = chisq.test(x = d1$Frequency[d1$trait == "Blast Resistance"]) # Select only Blast Resistance Trait Characteristics
blast # report out test results
blast$expected # See expected values if desired



leaf = chisq.test(x = d1$count[d1$trait == "Pigmentation at Leaf Juncture"], ) # Select only Blast Resistance Trait Characteristics
leaf # report out test results
leaf$expected 

# Because it is tedious to replicate the same code for each trait - I wrote a loop to make this easier

trait_list = unique(d1$trait) # Make list of all traits eligible for chi-squared test
trait_list
chisq_results = vector(mode = "list", length = length(trait_list)) # Make a results vector

# Loop
for (i in seq_along(trait_list)) {
  
  chisq = chisq.test(x = d1$Frequency[d1$trait == trait_list[i]]) # Define an object with chisq test results
  print(trait_list[i]) # print results
  
  # Identify if iteration has warning message
  warning_id = tryCatch(chisq.test(x = d1$Frequency[d1$trait == trait_list[i]]),
                        warning = function(w) {
                          print(w)
                        })
  
  # Define message
  warning_message = ifelse(is.null(warning_id$message), "None", "Chi-squared approximation may be incorrect")
  
  # Make results table for iteration
  results_df = tibble(trait = trait_list[i],
                      t_stat = chisq$statistic,
                      df = chisq$parameter,
                      p_value = chisq$p.value,
                      warning = warning_message)
  
  chisq_results[[i]] = results_df # put each iteration's df into a list
  
}

final = tibble(bind_rows(chisq_results)) # bind all results into 1 df

write.xlsx(final, file = "Chisquare_Final_analysis",rowNames=FALSE)
getwd()
