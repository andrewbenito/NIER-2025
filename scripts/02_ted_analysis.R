# 02_ted_plots.R analysis of Raw TED data
# Analysis of Conf Board cross-country Labour Productivity data

# see also
# https://www.productivity.ac.uk/the-productivity-lab/analysing-income-and-productivity-across-the-globe-the-conference-board-total-economy-database/

# Plots
# 1. Cross-country analysis of GDP per capita and growth rates from 1960 and 2000
# 2. UK vs Advanced Economies (AEs) in Output per Hour Worked growth
# 3. Population growth and Labour Productivity growth
# 4. Coefficients on Population growth and Convergence estimates

# create data ['dat'<tbl_df>]
source(here::here('scripts', '01x_create_data_latest_ted.R'))

# Options
opt.year <- 2025

# Convergence in Europe and Globally of Productivity from 2000

# Define the years you want
dat <- dat %>%
  group_by(country) %>%
  mutate(
    # initial level of labour productivity
    labor_productivity_per_hour_worked_1980 = labor_productivity_per_hour_worked[
      year == 1980
    ],
    labor_productivity_per_hour_worked_2000 = labor_productivity_per_hour_worked[
      year == 2000
    ],
    labor_productivity_per_hour_worked_2010 = labor_productivity_per_hour_worked[
      year == 2010
    ],
    labor_productivity_per_hour_worked_2019 = labor_productivity_per_hour_worked[
      year == 2019
    ]
  ) |>
  mutate(
    # cumulative growth in productivity
    labor_productivity_change_1980_2025 = log(labor_productivity_per_hour_worked[
      year == 2025
    ]) -
      log(labor_productivity_per_hour_worked[year == 2000]),
    labor_productivity_change_2010_2025 = log(labor_productivity_per_hour_worked[
      year == 2025
    ]) -
      log(labor_productivity_per_hour_worked[year == 2010]),
    labor_productivity_change_2000_2025 = log(labor_productivity_per_hour_worked[
      year == 2025
    ]) -
      log(labor_productivity_per_hour_worked[year == 2000])
  ) |>
  ungroup()

#===================

#  PLOTS

#===================
plot_data <- dat |> filter(Europe.x == 1 & year == opt.year)
# 1: Convergence plot: Europe, 2000-2025
avg_x <- mean(plot_data$labor_productivity_per_hour_worked_2000, na.rm = TRUE)
avg_y <- mean(plot_data$labor_productivity_change_2000_2025, na.rm = TRUE)

# Get plot limits to position labels
x_range <- range(
  plot_data$labor_productivity_per_hour_worked_2000,
  na.rm = TRUE
)
y_range <- range(plot_data$labor_productivity_change_2000_2025, na.rm = TRUE)

# PLOT
ted.plot_2000_2025 <- dat |>
  filter(Europe.x == 1 & year == opt.year) |>
  ggplot(aes(
    x = labor_productivity_per_hour_worked_2000,
    y = labor_productivity_change_2000_2025
  )) +
  geom_point() +
  # avg lines
  geom_hline(
    aes(yintercept = mean(labor_productivity_change_2000_2025, na.rm = TRUE)),
    color = "red",
    linetype = "dashed",
    alpha = 0.7
  ) +
  geom_vline(
    aes(
      xintercept = mean(labor_productivity_per_hour_worked_2000, na.rm = TRUE)
    ),
    color = "red",
    linetype = "dashed",
    alpha = 0.7
  ) +
  geom_smooth(method = lm, se = TRUE) +
  geom_text_repel(aes(label = country), hjust = -0.5) +
  # Add quadrant labels
  annotate(
    "text",
    x = avg_x + (x_range[2] - avg_x) * 0.9,
    y = avg_y + (y_range[2] - avg_y) * 0.9,
    label = "Steaming ahead",
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5,
    color = "darkblue",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = avg_x - (avg_x - x_range[1]) * 0.9,
    y = avg_y + (y_range[2] - avg_y) * 0.9,
    label = "Catching up",
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5,
    color = "darkblue",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = avg_x - (avg_x - x_range[1]) * 0.9,
    y = avg_y - (avg_y - y_range[1]) * 0.9,
    label = "Falling behind",
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5,
    color = "darkblue",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = avg_x + (x_range[2] - avg_x) * 0.9,
    y = avg_y - (avg_y - y_range[1]) * 0.9,
    label = "Losing ground",
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5,
    color = "darkblue",
    fontface = "bold"
  ) +
  labs(
    title = "European Productivity Convergence",
    subtitle = "Cumulative growth in labour productivity and initial value",
    caption = "Source: The Conference Board, Total Economy Database",
    x = "Output per hour worked, 2000",
    y = "Growth in labour productivity, 2000-2025, %"
  )
ted.plot_2000_2025
ggsave(
  here::here('figures', "ted.plot_2000_2025.png"),
  width = 5,
  height = 4,
  dpi = 300
)

# GLOBAL CONVERGENCE
plot_data <- dat |>
  filter(
    year == opt.year & grouped_country != 1
  )
# 1: Convergence plot: Global, 2000-2025
avg_x <- mean(plot_data$labor_productivity_per_hour_worked_2000, na.rm = TRUE)
avg_y <- mean(plot_data$labor_productivity_change_2000_2025, na.rm = TRUE)

# Get plot limits to position labels
x_range <- range(
  plot_data$labor_productivity_per_hour_worked_2000,
  na.rm = TRUE
)
y_range <- range(plot_data$labor_productivity_change_2000_2025, na.rm = TRUE)

# PLOT - Global
ted.global.plot_2000_2025 <- dat |>
  filter(year == opt.year) |>
  ggplot(aes(
    x = labor_productivity_per_hour_worked_2000,
    y = labor_productivity_change_2000_2025,
    color = factor(Europe.x)
  )) +
  geom_point() +
  # avg lines
  geom_hline(
    aes(yintercept = mean(labor_productivity_change_2000_2025, na.rm = TRUE)),
    color = "red",
    linetype = "dashed",
    alpha = 0.7
  ) +
  geom_vline(
    aes(
      xintercept = mean(labor_productivity_per_hour_worked_2000, na.rm = TRUE)
    ),
    color = "red",
    linetype = "dashed",
    alpha = 0.7
  ) +
  geom_smooth(method = lm, se = TRUE) +
  geom_text_repel(aes(label = country), hjust = -0.5) +
  # Add quadrant labels
  annotate(
    "text",
    x = avg_x + (x_range[2] - avg_x) * 0.9,
    y = avg_y + (y_range[2] - avg_y) * 0.9,
    label = "Steaming ahead",
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5,
    color = "darkblue",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = avg_x - (avg_x - x_range[1]) * 0.9,
    y = avg_y + (y_range[2] - avg_y) * 0.9,
    label = "Catching up",
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5,
    color = "darkblue",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = avg_x - (avg_x - x_range[1]) * 0.9,
    y = avg_y - (avg_y - y_range[1]) * 0.9,
    label = "Falling behind",
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5,
    color = "darkblue",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = avg_x + (x_range[2] - avg_x) * 0.9,
    y = avg_y - (avg_y - y_range[1]) * 0.9,
    label = "Losing ground",
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5,
    color = "darkblue",
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c("0" = "Gray", "1" = "blue"),
    labels = c("0" = "Ex-Europe", "1" = "Europe"),
    name = ""
  ) +
  labs(
    title = "Global Productivity Convergence",
    subtitle = "Cumulative growth in labour productivity and initial value",
    caption = "Source: The Conference Board, Total Economy Database",
    x = "Output per hour worked, 2000",
    y = "Growth in labour productivity, 2000-2025, %"
  )
ted.global.plot_2000_2025
ggsave(
  here::here('figures', "ted.global.plot_2000_2025.png"),
  width = 5,
  height = 4,
  dpi = 300
)

#=================================

# Contributions / Decompositions

#=================================
# Decomposition 2010-2025

# Productivity growth and Population Growth

# 1b: From 2000
ggplot(df2019, aes(x = gdppc2000, y = g20002019)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  geom_text_repel(aes(label = ISO), hjust = -0.5) +
  xlab("GDP per capita, 2000") +
  ylab("Growth rate, 2000-2019 (annual rate, %)")
ggsave(here::here('figures', "Fig_1b.png"), width = 5, height = 4, dpi = 300)

# 2. Ribbon Chart, UK and Max and Min within AEs
ggplot(df3, aes(x = year)) +
  geom_ribbon(
    aes(ymin = dlyhMin, ymax = dlyhMax),
    alpha = 0.2,
    linetype = 1,
    fill = "red"
  ) +
  geom_line(
    data = subset(df3, ISO %in% c("GBR")),
    aes(y = `Output per Hour Worked growth`)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(
    breaks = seq(
      min(df3$year, na.rm = TRUE),
      max(df3$year, na.rm = TRUE) + 1,
      by = 10
    )
  ) +
  scale_color_jco() +
  xlab("Year") +
  ylab("Growth in Output per Hour Worked, %pa") +
  labs(
    title = "Labour Productivity Growth: UK and Advanced economies",
    caption = "Source: Conference Board"
  ) +
  theme(plot.caption = element_text(hjust = 0))
ggsave(here::here('figures', "Fig_2.png"), width = 5, height = 4, dpi = 300)

# 3. Population growth and Labour Productivity
ggplot(
  byISO,
  aes(x = `Population growth`, y = `Output per Employed Person growth`)
) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_text_repel(aes(label = ISO), hjust = -0.5) +
  xlab("Population growth (avg annual rate, %)") +
  ylab("Growth in Output per Person Employed (avg annual rate, %)")
ggsave(here::here('figures', "Fig_3a.png"), width = 5, height = 4, dpi = 300)

mod1 <- lm(
  `Output per Employed Person growth` ~ `Population growth`,
  data = byISO
)
summary(mod1)

# Population growth by Sub-period
ggplot(
  byISOTime,
  aes(x = `Population growth`, y = `Output per Employed Person growth`)
) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~decade) +
  geom_text_repel(
    aes(label = ifelse(ISO == "GBR", ISO, '')),
    hjust = -0.5,
    max.overlaps = Inf
  ) +
  xlab("Population growth (avg annual rate, %)") +
  ylab("Growth in Output per Person Employed (avg annual rate, %)")
ggsave(here::here('figures', "Fig_3b.png"), width = 5, height = 4, dpi = 300)


# 4. Plot Coefficients ----
# Population Growth over Time
byYear %>%
  unnest(tidied) %>%
  dplyr::filter(term == 'dPop20y') %>%
  mutate(
    conf.low_90 = estimate - 1.645 * std.error,
    conf.high_90 = estimate + 1.645 * std.error
  ) %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_point(aes(
    x = year,
    y = estimate,
    color = "Estimate with 90% conf. interval"
  )) +
  geom_linerange(
    aes(x = year, ymin = conf.low_90, ymax = conf.high_90),
    lwd = 1,
    color = "dodgerblue",
    show.legend = T
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Coefficient on Population growth over time",
    x = "Final year (20-year sample)",
    y = "Coefficient on Pop growth"
  ) +
  theme(legend.title = element_blank(), legend.position = "bottom")
ggsave(here::here('figures', "Fig_4.png"), width = 5, height = 4, dpi = 300)

# Convergence Estimate over Time
plot_fig4 <- byYear %>%
  unnest(tidied) %>%
  filter(term == "ylinitial") %>%
  mutate(
    conf.low_90 = estimate - 1.645 * std.error,
    conf.high_90 = estimate + 1.645 * std.error
  ) %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_point(color = "black") +
  geom_linerange(
    aes(ymin = conf.low_90, ymax = conf.high_90),
    color = "dodgerblue",
    linewidth = 1
  ) +
  labs(
    x = "Final year (20-year sample)",
    y = "Convergence estimate (%pa)",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "none"
  )
plot_fig4
# Save the plot
ggsave(
  filename = here::here("figures", "Fig_4.png"),
  plot = plot_fig4,
  width = 5,
  height = 4,
  dpi = 300
)


# Coefficients on Pop Growth by Country
byCountry %>%
  unnest(tidied) %>%
  dplyr::filter(term == 'dPop20y') %>%
  mutate(
    conf.low_90 = estimate - 1.645 * std.error,
    conf.high_90 = estimate + 1.645 * std.error,
    ISO = fct_reorder(ISO, estimate)
  ) %>%
  ggplot(aes(x = ISO, y = estimate)) +
  geom_point(aes(
    x = ISO,
    y = estimate,
    color = "Estimate with 90% conf. interval"
  )) +
  geom_linerange(
    aes(x = ISO, ymin = conf.low_90, ymax = conf.high_90),
    lwd = 1,
    color = "dodgerblue",
    show.legend = T
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Country") +
  ylab("Coefficient on Pop growth") +
  theme(legend.title = element_blank(), legend.position = "bottom")

# Coefficients on yInitial by Country
byCountry %>%
  unnest(tidied) %>%
  filter(term == 'ylinitial') %>%
  mutate(
    conf.low_90 = estimate - 1.645 * std.error,
    conf.high_90 = estimate + 1.645 * std.error,
    ISO = fct_reorder(ISO, estimate)
  ) %>%
  ggplot(aes(x = ISO, y = estimate)) +
  geom_point(aes(
    x = ISO,
    y = estimate,
    color = "Estimate with 90% conf. interval"
  )) +
  geom_linerange(
    aes(x = ISO, ymin = conf.low_90, ymax = conf.high_90),
    lwd = 1,
    color = "dodgerblue",
    show.legend = T
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Country") +
  ylab("Coefficient on Initial Productivity") +
  theme(legend.title = element_blank(), legend.position = "bottom")
