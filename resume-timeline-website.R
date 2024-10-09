# Install and load required packages
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")

library(ggplot2)
library(dplyr)
library(lubridate)

# Create data frame for timeline events
data <- tribble(
  ~content, ~start, ~end, ~group,
  "BS Industrial & Systems Engineering (UW-Madison)", "2016-09-01", "2021-05-31", "Education",
  "MS Industrial & Systems Engineering (UW)", "2021-09-01", "2023-03-31", "Education",
  "PhD Transportation Systems (NYU)", "2023-09-01", "2027-05-31", "Education",
  "UW-Madison Cognitive Systems Lab", "2017-05-01", "2019-08-31", "Research",
  "UW Human Factors and Statistical Modeling Lab", "2021-06-01", "2023-08-31", "Research",
  "NYU Human Factors & Urban Ergonomics Lab", "2023-09-01", "2027-05-31", "Research",
  "ICR, Inc. (Intern)", "2019-05-01", "2019-07-31", "Industry",
  "Seagate Technology (Intern)", "2020-06-01", "2021-08-31", "Industry",
  "NCAA Division 1 Soccer (UW-Madison)", "2016-09-01", "2021-05-31", "Sports"
) %>%
  mutate(start = ymd(start), end = ymd(end))

# Create data frame for publications (point events)
publications <- tribble(
  ~content, ~date, ~group,
  "Proxemics and Kinesics Paper", "2019-01-01", "Publications",
  "Framework to Assess Pedestrian Exposure", "2022-01-01", "Publications",
  "Pedestrian Exposure Using GPS Data", "2024-01-01", "Publications",
  "Multimodal Environments Paper", "2024-06-01", "Publications",
  "TRB Annual Meeting Presentation", "2025-01-01", "Publications"
) %>%
  mutate(date = ymd(date))

# Set up a nicer color palette
color_palette <- c(
  "Education" = "#4e79a7",
  "Research" = "#f28e2b",
  "Industry" = "#59a14f",
  "Sports" = "#e15759",
  "Publications" = "#76b7b2"
)

# Create the plot
p <- ggplot() +
  # Add timeline segments
  geom_segment(data = data, aes(x = stzxart, xend = end, y = group, yend = group, color = group), size = 8) +
  # Add publication points
  geom_point(data = publications, aes(x = date, y = group, color = group), size = 4) +
  # Add text labels for timeline segments
  geom_text(data = data, aes(x = start + (end - start)/2, y = group, label = content), 
            vjust = -0.8, hjust = 0.5, angle = 0, size = 3, fontface = "bold") +
  # Add text labels for publications
  geom_text(data = publications, aes(x = date, y = group, label = content), 
            vjust = 2, hjust = 0.5, angle = 0, size = 3, fontface = "bold") +
  # Customize the theme
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  ) +
  # Set color scheme
  scale_color_manual(values = color_palette) +
  # Set x-axis limits and breaks
  scale_x_date(limits = c(ymd("2016-01-01"), ymd("2027-12-31")),
               date_breaks = "1 year", date_labels = "%Y") +
  # Set y-axis limits to add more space for labels
  scale_y_discrete(limits = rev(c("Education", "Research", "Industry", "Sports", "Publications")),
                   expand = expansion(add = c(0.5, 1.2))) +
  # Add title
  ggtitle("Grace Douglas's Career Timeline") +
  # Adjust the aspect ratio
  theme(aspect.ratio = 0.4)

# Display the plot
print(p)

# Save the plot as a high-resolution PNG file
ggsave("career_timeline.png", plot = p, width = 18, height = 10, dpi = 300)