
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggrepel)  # For smarter label placement
#rmarkdown::clean_site(preview = FALSE)
#rmarkdown::render_site()
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
  "Seagate Technology (Intern)", "2020-06-01", "2021-08-31", "Industry"
) %>%
  mutate(start = ymd(start), end = ymd(end))

publications <- tribble(
  ~content, ~date, ~group,
  "Proxemics and Kinesics Paper", "2019-01-01", "Publications",
  "Framework to Assess Pedestrian Exposure", "2022-01-01", "Publications",
  "Pedestrian Exposure Using GPS Data", "2024-01-01", "Publications",
  "Multimodal Environments Paper", "2024-06-01", "Publications",
  "TRB Annual Meeting Presentation", "2025-01-01", "Publications"
) %>%
  mutate(date = ymd(date))

color_palette <- c(
  "Education" = "#B5D3E7",
  "Research" = "#C7E5C8",
  "Industry" = "#FFDAB9",
  "Publications" = "#E6C3D4"
)

publications <- publications %>%
  arrange(date) %>%
  mutate(label_position = rep(c(1, -1), length.out = n()))

library(ggplot2)
library(dplyr)
library(lubridate)
library(ggrepel)

# Assuming your data preparation code remains the same

p <- ggplot() +
  geom_segment(data = data, aes(x = start, xend = end, y = group, yend = group, color = group), 
               linewidth = 16, alpha = 0.9) +  # Increased linewidth and alpha
  geom_point(data = publications, aes(x = date, y = group, color = group), size = 28, alpha = 0.9) +  # Increased size and alpha
  geom_text_repel(data = data, aes(x = start + (end - start)/2, y = group, label = content, color = group),
                  size = 16, fontface = "bold", box.padding = 0.7, point.padding = 0.5,
                  segment.color = "grey50", force = 15, max.overlaps = Inf, 
                  direction = "y", nudge_y = 0.4, alpha = 1) +  # Increased size and force
  geom_text_repel(data = publications, aes(x = date, y = group, label = content, color = group),
                  size = 16, fontface = "bold", box.padding = 0.7, point.padding = 0.5,
                  segment.color = "grey50", force = 20, direction = "both", 
                  nudge_y = publications$label_position * 0.4,
                  max.overlaps = Inf,
                  segment.size = 0.5,
                  segment.curvature = -0.1,
                  min.segment.length = 0,
                  max.time = 2,
                  max.iter = 3000,
                  alpha = 1) +  # Increased size, force, and iterations
  theme_minimal(base_size = 24) +  # Increased base font size
  theme(
    axis.text.y = element_text(size = 38, face = "bold"),  # Increased size
    axis.text.x = element_text(size = 34),  # Increased size
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 36, face = "bold", margin = margin(b = 20)),  # Increased size and added bottom margin
    plot.margin = margin(t = 50, r = 50, b = 50, l = 50, unit = "pt"),  # Increased margins
    text = element_text(color = "black")
  ) +
  scale_color_manual(values = color_palette) +
  scale_x_date(limits = c(ymd("2016-01-01"), ymd("2028-01-01")),
               date_breaks = "1 year", date_labels = "%Y") +
  scale_y_discrete(limits = rev(c("Education", "Research", "Publications", "Industry")),
                   expand = expansion(mult = c(0.3, 0.4))) +  # Increased vertical expansion
  theme(aspect.ratio = 0.4)  # Increased aspect ratio for a taller plot

print(p)

# Adjusted ggsave parameters
ggsave("~/Documents/arXiv/grackith.github.io/images/timeline.png", 
       width = 70, height = 30, plot = p, dpi = 300, limitsize = FALSE)  # Increased both width and height

