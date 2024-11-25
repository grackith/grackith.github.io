library(ggplot2)
library(dplyr)
library(lubridate)
library(ggrepel)
library(stringr)
library(ggforce) # Added for rounded rectangles


# Darker blue color palette
color_palette <- c(
  "Education" = "#08519c", # Darker blue
  "Research" = "#3182bd", # Medium-dark blue
  "Industry" = "#6baed6", # Medium blue (darkened from before)
  "Publications" = "#2171b5" # Blue-medium dark
)
# Helper function to wrap text
wrap_text <- function(text, width = 30) {
  str_wrap(text, width = width)
}

# Update the industry text to fit boxes better
data <- tribble(
  ~content, ~start, ~end, ~group,
  "B.Sc. ISyE (UW-Madison)", "2016-09-01", "2021-05-31", "Education",
  "M.Sc. ISE (UW)", "2021-09-01", "2023-03-31", "Education",
  "Ph.D. Transportation Systems (NYU Tandon)", "2023-09-01", "2027-05-31", "Education", # nolint: line_length_linter.
  "Cognitive Systems Lab\nAdvisor: Dr. John Lee", "2017-05-01", "2019-08-31", "Research", # nolint: line_length_linter.
  "HFSM Lab\nAdvisor: Dr. Linda Ng Boyle", "2021-06-01", "2023-08-31", "Research", # nolint: line_length_linter.
  "HumanFUEL\nAdvisor: Dr. Linda Ng Boyle", "2023-09-01", "2027-05-31", "Research", # nolint: line_length_linter.
  "Data Science\nIntern (ICR)", "2019-02-01", "2020-01-31", "Industry",
  "Manufacturing & Supply\nChain Intern (Seagate)", "2020-04-01", "2021-12-01", "Industry" # nolint: line_length_linter.
) %>%
  mutate(start = ymd(start), end = ymd(end))

publications <- tribble(
  ~content, ~date, ~group, ~nudge_x, ~nudge_y,
  "Proxemics and Kinesics in\nAutomated Vehicle-Pedestrian\nCommunication (TRR)", "2019-01-01", "Publications", -300, -0.4, # nolint: line_length_linter.
  "Framework to Assess\nPedestrian Exposure (HFES)", "2022-01-01", "Publications", 300, 0.4, # nolint: line_length_linter.
  "Comparing Pedestrian\nExposure Measures (TRB)", "2024-01-01", "Publications", -400, -0.4, # nolint: line_length_linter.
  "Examining drivers' secondary task\nengagement at intersections using\nnaturalistic driving data (AAP)", "2024-10-01", "Publications", 100, -0.1, # nolint: line_length_linter.
  "Misclassications in rare\nevent prediction (TR-F)", "2025-01-01", "Publications", -500, -0.6 # nolint: line_length_linter.
) %>%
  mutate(date = ymd(date))

p <- ggplot() +
  geom_rect(
    data = data,
    aes(
      xmin = start,
      xmax = end,
      ymin = as.numeric(factor(group, levels = rev(c("Education", "Research", "Publications", "Industry")))) - 0.35, # nolint: line_length_linter. # nolint
      ymax = as.numeric(factor(group, levels = rev(c("Education", "Research", "Publications", "Industry")))) + 0.35, # nolint
      color = group
    ),
    fill = NA,
    linewidth = 0.2
  ) +
  # Direct labels for timeline segments
  geom_text(
    data = data %>% filter(group != "Publications"),
    aes(
      x = start + (end - start) / 2,
      y = group,
      label = content,
      color = group
    ),
    size = 4.5,
    fontface = "plain", # Changed from bold
    lineheight = 0.8
  ) +
  # Publication points
  geom_point(
    data = publications,
    aes(x = date, y = group, color = group),
    size = 10,
    alpha = 0.9
  ) +
  geom_text_repel(
    data = publications,
    aes(x = date, y = group, label = content, color = group),
    size = 4.5,
    fontface = "plain",
    lineheight = 0.8,
    box.padding = 1,
    point.padding = 0.5,
    force = 20,
    direction = "both",
    nudge_x = publications$nudge_x,
    nudge_y = publications$nudge_y,
    segment.size = 0.3,
    segment.color = "grey50",
    segment.alpha = 0.8,
    min.segment.length = 0.3,
    max.overlaps = Inf,
    segment.curvature = -0.4, # Increased curve
    segment.angle = 45, # Adjusted angle for better pointing
    segment.ncp = 15, # More control points for smoother curves
    arrow = arrow(
      length = unit(0.012, "npc"),
      type = "closed",
      ends = "last"
    ), # Arrow at label end
    hjust = 0.5
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.y = element_text(size = 16, face = "plain"), # Changed from bold
    axis.text.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
    panel.grid.major.x = element_line(color = "grey90", size = 0.3)
  ) +
  scale_color_manual(values = color_palette) +
  scale_x_date(
    limits = c(ymd("2016-01-01"), ymd("2028-01-01")),
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_discrete(
    limits = rev(c("Education", "Research", "Publications", "Industry")),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  theme(aspect.ratio = 0.22)

ggsave("/Users/gracedouglas/Documents/arXiv/grackith.github.io/images/timeline.png", p, width = 16, height = 4, dpi = 300) # nolint: line_length_linter.
