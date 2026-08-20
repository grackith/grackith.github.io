# Builds images/timeline.png for timeline.Rmd.
# Run from the repo root:  Rscript scripts/resume-timeline-website.R

library(ggplot2)
library(dplyr)
library(lubridate)
library(ggrepel)
library(stringr)

out_file <- "images/timeline.png"

color_palette <- c(
  "Education"    = "#08519c", # darkest blue
  "Research"     = "#3182bd",
  "Industry"     = "#6baed6",
  "Publications" = "#2171b5"
)

# Explicit lane positions, top to bottom. Publications sits last so its
# callout labels can fan downward into empty space instead of colliding with
# the career bars. Research carries two sub-tracks because HumanFUEL and the
# Emory appointment run concurrently and would otherwise overlap.
lane_y_pos <- c("Education" = 5.0, "Research" = 3.6, "Industry" = 2.2, "Publications" = 1.0)
lane_order <- names(lane_y_pos)

data <- tribble(
  ~content, ~start, ~end, ~group, ~y_off, ~half_h,
  "B.Sc. ISyE (UW-Madison)", "2016-09-01", "2021-05-31", "Education", 0, 0.28,
  "M.Sc. ISE (UW)", "2021-09-01", "2023-03-31", "Education", 0, 0.28,
  "Ph.D. Transportation Systems (NYU Tandon)", "2023-09-01", "2027-05-31", "Education", 0, 0.28,
  "Cognitive Systems Lab\nAdvisor: Dr. John Lee", "2017-05-01", "2019-08-31", "Research", 0, 0.28,
  "HFSM Lab\nAdvisor: Dr. Linda Ng Boyle", "2021-06-01", "2023-08-31", "Research", 0, 0.28,
  "HumanFUEL\nAdvisor: Dr. Linda Ng Boyle", "2023-09-01", "2027-05-31", "Research", 0.34, 0.29,
  "Lakdawala Lab (Emory)\nComputer Vision", "2026-01-01", "2027-05-31", "Research", -0.36, 0.28,
  "Data Science\nIntern (ICR)", "2019-02-01", "2020-01-31", "Industry", 0, 0.28,
  "Manufacturing & Supply\nChain Intern (Seagate)", "2020-04-01", "2021-12-01", "Industry", 0, 0.28
) %>%
  mutate(start = ymd(start), end = ymd(end), y = lane_y_pos[group] + y_off)

# Publications & conference presentations.
# status drives point shape: solid = out, hollow = accepted / in preparation.
publications <- tribble(
  ~content, ~date, ~status, ~nudge_x, ~nudge_y,
  "Proxemics & Kinesics in AV-\nPedestrian Communication (TRR)", "2019-10-01", "Published", 0, -0.9,
  "Framework to Assess Pedestrian\nExposure (HFES)", "2022-09-01", "Published", 0, -0.9,
  "Pedestrian Exposure Framework\n(NHTSA DOT HS 813 583)", "2024-06-01", "Published", -600, -1.9,
  "Drivers' Secondary Task\nEngagement at Intersections", "2024-11-01", "Published", -250, -4.5,
  "Pedestrian Exposure Measures\nAcross Scales (TRB)", "2025-01-15", "Published", -1150, -5.6,
  "Virtual Worlds for Real Agents\n(AutomotiveUI)", "2025-09-01", "Published", -450, -3.2,
  "Misclassification Framework for\nPedestrian Crash Prediction (AAP)", "2026-01-01", "Published", 250, -1.9,
  "Distracted Driver Behavior\nTypology (AAP)", "2026-06-01", "In press / in prep", 300, -3.0,
  "Systematic Misclassification in\nCrash Modeling (JSM poster)", "2026-08-01", "Published", 300, -4.1,
  "So Pedestrian: AV Detection\nDisplays (AutomotiveUI)", "2026-09-01", "In press / in prep", 300, -5.2,
  "Decoupling Pose from Identity\n(PLOS Biology)", "2026-12-01", "In press / in prep", 250, -6.3
) %>%
  mutate(date = ymd(date), group = "Publications", y = lane_y_pos[group])

p <- ggplot() +
  geom_rect(
    data = data,
    aes(
      xmin = start, xmax = end,
      ymin = y - half_h,
      ymax = y + half_h,
      color = group
    ),
    fill = NA,
    linewidth = 0.25
  ) +
  geom_text(
    data = data,
    aes(x = start + (end - start) / 2, y = y, label = content, color = group),
    size = 4.0,
    lineheight = 0.85
  ) +
  # white halo first, so points one month apart still read as two points
  geom_point(
    data = publications,
    aes(x = date, y = y),
    size = 5.4,
    color = "white"
  ) +
  geom_point(
    data = publications,
    aes(x = date, y = y, fill = status),
    shape = 21,
    color = "#2171b5",
    stroke = 1.0,
    size = 3.4
  ) +
  geom_text_repel(
    data = publications,
    aes(x = date, y = y, label = content, color = group),
    size = 3.6,
    lineheight = 0.85,
    box.padding = 0.45,
    point.padding = 0.35,
    force = 4,
    force_pull = 1.1,
    direction = "both",
    nudge_x = publications$nudge_x,
    nudge_y = publications$nudge_y,
    segment.size = 0.3,
    segment.color = "grey55",
    segment.alpha = 0.9,
    min.segment.length = 0,
    max.overlaps = Inf,
    segment.curvature = -0.2,
    segment.ncp = 10,
    seed = 42
  ) +
  scale_color_manual(values = color_palette, guide = "none") +
  scale_fill_manual(
    values = c("Published" = "#2171b5", "In press / in prep" = "white"),
    breaks = c("Published", "In press / in prep"),
    name = NULL
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 3.6, color = "#2171b5"))) +
  scale_x_date(
    limits = c(ymd("2016-01-01"), ymd("2027-09-30")),
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = unname(lane_y_pos),
    labels = lane_order,
    limits = c(-5.9, 5.6)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 11),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 11),
    plot.margin = margin(t = 6, r = 12, b = 6, l = 6, unit = "pt")
  )

ggsave(out_file, p, width = 16, height = 7.4, dpi = 200, bg = "white")
cat("wrote", out_file, "\n")
