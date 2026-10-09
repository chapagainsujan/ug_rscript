## Load required libraries

library(ggplot2)
library(dplyr)
library(patchwork)
library(showtext)

## Assign the input dataset

data <- Graph

## Enable custom font rendering

showtext_auto()

## Set text sizes for chart titles, percentage labels, and legends

title_size <- 70
label_size <- 23
legend_size <- 60

## Define a custom color palette for pie-chart categories

custom_colors <- c(
"#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
"#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
"#86BCB6", "#FABFD2", "#CFCFCF", "#8CD17D", "#D4A6C8"
)

## Define a function to create a pie chart for each trait

create_pie_chart <- function(data, trait_name, label_tag) {

## Filter the dataset for the selected trait and calculate percentages

df <- data %>%
filter(Trait == trait_name) %>%
mutate(
Percentage = Frequency / sum(Frequency) * 100
) %>%

```
## Arrange categories in descending alphabetical order
arrange(desc(Character)) %>%

## Calculate segment midpoints and hide labels for segments below 5%
mutate(
  cumulative = cumsum(Percentage) - (Percentage / 2),
  Label = ifelse(
    Percentage >= 5,
    paste0(round(Percentage), "%"),
    ""
  )
)
```

## Create the pie chart using percentage values

ggplot(df, aes(x = "", y = Percentage, fill = Character)) +
geom_bar(stat = "identity", width = 1, color = "white") +
coord_polar(theta = "y") +
theme_void() +

```
## Position percentage labels at the midpoint of each segment
geom_text(
  aes(y = cumulative, label = Label),
  size = label_size,
  family = "Times New Roman",
  color = "black"
) +

## Add the panel label and trait name as the chart title
ggtitle(paste0(label_tag, " ", trait_name)) +

## Customize chart titles and legend text
theme(
  plot.title = element_text(
    hjust = 0.7,
    family = "Times New Roman",
    size = title_size
  ),
  legend.title = element_blank(),
  legend.text = element_text(
    family = "Times New Roman",
    size = legend_size
  )
) +

## Apply the predefined category colors
scale_fill_manual(values = custom_colors)
```

}

## Extract unique traits and generate alphabetical panel labels

traits <- unique(Graph$Trait)
labels <- letters[1:length(traits)]

## Generate a pie chart for each trait

plots <- mapply(
function(trait, label_tag) {
create_pie_chart(Graph, trait, paste0(label_tag, "."))
},
traits,
labels,
SIMPLIFY = FALSE
)

## Divide the pie charts into two rows

n <- length(plots)
half <- ceiling(n / 2)

## Select the plots for each row

row1 <- plots[1:half]
row2 <- plots[(half + 1):n]

## Create an empty plot to separate the two rows

spacer <- ggplot() + theme_void()

## Arrange the charts horizontally in each row

row1_plot <- wrap_plots(row1, nrow = 1)
row2_plot <- wrap_plots(row2, nrow = 1)

## Combine both rows and adjust the spacing

final_plot <- (row1_plot / spacer / row2_plot) +
plot_layout(heights = c(1, 0.15, 1))

## Save the combined figure as a high-resolution TIFF file

ggsave(
"Pie_Chartsgrowth.tiff",
plot = final_plot,
width = 12,
height = 6,
dpi = 600,
compression = "lzw"
)
