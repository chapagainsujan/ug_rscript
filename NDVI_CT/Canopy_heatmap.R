library(tidyverse)
library(readxl)
library(stringr)
library(patchwork)

# Read in data from each sheet. I found a data entry error, and fixed it 



irrigated = read_xlsx("Research/Sandesh Field/NDVI and Canopy Temperature/CCanopy Temperature.xlsx", sheet= 1)


drought = read_xlsx("Research/Sandesh Field/NDVI and Canopy Temperature/CCanopy Temperature.xlsx" ,sheet = 2)


# bind dataframes together
# make variables lower case, per my preference

d = bind_rows(drought, irrigated) %>%
  mutate(across(everything(), tolower)) %>% # I like working in lower case. Personal preference
  rename_with(tolower) %>% # column names too
  rename(d30 = `30d`,
         d45 = `45d`,
         d60 = `60d`,
         d75 = `75d`,
         d90 = `90d`,
         d105 = `105d`) # starting character variables with numbers can cause complications, so i rename

# Every unique 'gen' will need a location on a grid for this visual. 
# We want 8 rows and there is 24 'gen', so lets use an 8x3 grid
# Read in .xlsx file for that maps each 'gen' to its location on the grid



grid=grid
# Now, we need to make our data long. We want each duration to be in the same column

d1 = d %>%
  left_join(grid, by = "gen") %>% # Merge the coordinate data from our "grid"
  pivot_longer(cols = c(d30,d45,d60,d75, d90, d105), 
               names_to = "duration", 
               values_to = "CT") %>% # Make data longer
  mutate(CT = as.numeric(CT)) %>% # ndvi values came in as character, change to numeric
  mutate(trt = str_to_title(trt)) %>% # Make first letter capital
  mutate(rep = ifelse(rep == 1, "Rep 1", "Rep 2"))

# This plotting is a bit complicated. Basically, we make a separate plot for each duration,
# then arrange into a grid after. Below is a list, with the plots for all 4 durations

day_30 = d1 %>%
  filter(duration == "d30") %>% # Filter to day 30
  ggplot(aes(x = x, y = y, fill = CT)) + # "aes" stands for aesthetic. All variables take from data need to be in aes()
  geom_tile() + # Plot tiles
  facet_grid(rows = vars(trt), cols = vars(rep), # Facet by trt and rep variables
             scales="free_y", switch = 'both') + # flip the strip.text labels
  scale_fill_gradient(low = "#FFFFFF", high = "#FF0000") + # Manually the gradient to green
  ggtitle("A. Tillering") + # Add title
  guides(fill="none") + #remove legend
  theme_void() +
  theme(
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 11), # Bold and smaller title
    strip.text.y = element_text(family = "Times New Roman", angle = 90, size = 9, vjust = 1, 
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    strip.text.x = element_text(family = "Times New Roman", size = 9,
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    panel.background = element_rect(color = "black", fill = NA),
    panel.spacing = unit(.5, 'pt'))
day_30



day_45 = d1 %>%
  filter(duration == "d45") %>%
  ggplot(aes(x = x, y = y, fill = CT)) +
  geom_tile() +
  facet_grid(rows = vars(trt), cols = vars(rep), 
             scales="free_y", switch = 'both') +
  scale_fill_gradient(low = "#FFFFFF", high = "#FF0000") +
  ggtitle("B. Jointing") +
  guides(fill="none") +
  theme_void() +
  theme(
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 11), # Bold and smaller title
    strip.text.y = element_text(family = "Times New Roman", angle = 90, size = 9, vjust = 1, 
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    strip.text.x = element_text(family = "Times New Roman", size = 9,
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    panel.background = element_rect(color = "black", fill = NA),
    panel.spacing = unit(.5, 'pt'))

day_60 = d1 %>%
  filter(duration == "d60") %>%
  ggplot(aes(x = x, y = y, fill = CT)) +
  geom_tile() +
  facet_grid(rows = vars(trt), cols = vars(rep), 
             scales="free_y", switch = 'both') +
  scale_fill_gradient(low = "#FFFFFF", high = "#FF0000") +
  ggtitle("c. Booting") +
  guides(fill="none") +
  theme_void() +
  theme(
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 11), # Bold and smaller title
    strip.text.y = element_text(family = "Times New Roman", angle = 90, size = 9, vjust = 1, 
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    strip.text.x = element_text(family = "Times New Roman", size = 9,
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    panel.background = element_rect(color = "black", fill = NA),
    panel.spacing = unit(.5, 'pt'))
day_75 = d1 %>%
  filter(duration == "d75") %>%
  ggplot(aes(x = x, y = y, fill = CT)) +
  geom_tile() +
  facet_grid(rows = vars(trt), cols = vars(rep), 
             scales="free_y", switch = 'both') +
  scale_fill_gradient(low = "#FFFFFF", high = "#FF0000") +
  ggtitle("D. Heading") +
  theme_void() +
  theme(
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 11), # Bold and smaller title
    strip.text.y = element_text(family = "Times New Roman", angle = 90, size = 9, vjust = 1, 
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    strip.text.x = element_text(family = "Times New Roman", size = 9,
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    panel.background = element_rect(color = "black", fill = NA),
    panel.spacing = unit(.5, 'pt')
  )


day_90 = d1 %>%
  filter(duration == "d75") %>%
  ggplot(aes(x = x, y = y, fill = CT)) +
  geom_tile() +
  facet_grid(rows = vars(trt), cols = vars(rep), 
             scales="free_y", switch = 'both') +
  scale_fill_gradient(low = "#FFFFFF", high = "#FF0000") +
  ggtitle("E. Anthesis") +
  theme_void() +
  guides(fill="none") +
  
  theme(
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 11), # Bold and smaller title
    strip.text.y = element_text(family = "Times New Roman", angle = 90, size = 9, vjust = 1, 
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    strip.text.x = element_text(family = "Times New Roman", size = 9,
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    panel.background = element_rect(color = "black", fill = NA),
    panel.spacing = unit(.5, 'pt')
  )
day_105 = d1 %>%
  filter(duration == "d105") %>%
  ggplot(aes(x = x, y = y, fill = CT)) +
  geom_tile() +
  facet_grid(rows = vars(trt), cols = vars(rep), 
             scales="free_y", switch = 'both') +
  scale_fill_gradient(low = "#FFFFFF", high = "#FF0000") +
  ggtitle("F. Soft Dough") +
  theme_void() +
  guides(fill="none") +
  
  theme(
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 11), # Bold and smaller title
    strip.text.y = element_text(family = "Times New Roman", angle = 90, size = 9, vjust = 1, 
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    strip.text.x = element_text(family = "Times New Roman", size = 9,
                                margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5)),
    panel.background = element_rect(color = "black", fill = NA),
    panel.spacing = unit(.5, 'pt')
  )

# Visualize our list of plots
# Guides = "collect" uses only one legend

aa= day_30 + day_45 + day_60 + day_75 + day_90+ day_105 + patchwork::plot_layout(nrow = 3, ncol = 2, guides = "collect") 
aa
ggsave(filename = "Canopy Temperature.png", plot = aa,width = 25, height = 20, dpi = 500, units = "cm")
