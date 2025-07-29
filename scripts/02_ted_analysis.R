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

# Define
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

# Data for Decompositions Analysis of GDP growth
# Period: 2010-2025

decomp <- dat %>%
  group_by(country) %>%
  mutate(
    gdp_contr_labor_quant_sum_2010_2025 = sum(
      contribution_of_labor_quantity_to_real_gdp_growth[
        year >= 2010 & year <= 2025
      ],
      na.rm = TRUE
    ),
    gdp_contr_labor_qual_sum_2010_2025 = sum(
      contribution_of_labor_quality_to_real_gdp_growth[
        year >= 2010 & year <= 2025
      ],
      na.rm = TRUE
    ),
    gdp_contr_cap_ict_sum_2010_2025 = sum(
      contribution_of_capital_services_provided_by_ict_assets_to_real_gdp_growth[
        year >= 2010 & year <= 2025
      ],
      na.rm = TRUE
    ),
    gdp_contr_cap_nonict_sum_2010_2025 = sum(
      contribution_of_capital_services_provided_by_non_ict_assets_to_real_gdp_growth[
        year >= 2010 & year <= 2025
      ],
      na.rm = TRUE
    ),
    gdp_contr_tfp_sum_2010_2025 = sum(
      growth_of_total_factor_productivity[year >= 2010 & year <= 2025],
      na.rm = TRUE
    ),
    gdp_growth_sum_2010_2025 = sum(
      growth_in_real_gdp.x[year >= 2010 & year <= 2025],
      na.rm = TRUE
    )
  ) %>%
  ungroup()

decomp_plot <- decomp |>
  filter(Europe.x == 1) |>
  group_by(country) |>
  slice(1) |>
  ungroup() |>
  select(
    country,
    `Labour quality` = gdp_contr_labor_qual_sum_2010_2025,
    `Labour quantity` = gdp_contr_labor_quant_sum_2010_2025,
    `ICT Capital` = gdp_contr_cap_ict_sum_2010_2025,
    `Non-ICT Capital` = gdp_contr_cap_nonict_sum_2010_2025,
    `TFP` = gdp_contr_tfp_sum_2010_2025
  ) |>
  pivot_longer(
    cols = -country,
    names_to = "component",
    values_to = "contribution"
  )

# Pop growth and Productivity
#-----------------------------
dat <- dat %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    # Population growth rate 2000-2025 (with error handling)
    popg_0025 = ifelse(
      !is.na(midyear_population[year == 2000]) &
        !is.na(midyear_population[year == 2025]) &
        midyear_population[year == 2000] > 0,
      ((midyear_population[year == 2025] /
        midyear_population[year == 2000])^(1 / 25) -
        1) *
        100,
      NA
    ),

    # Log differences (annual growth rates)
    dlyh = log(labor_productivity_per_hour_worked) -
      log(lag(labor_productivity_per_hour_worked)),
    dlpop = log(midyear_population) - log(lag(midyear_population)),

    # Log levels
    lyh = log(labor_productivity_per_hour_worked),
    lyh1990 = ifelse(
      any(year == 1990, na.rm = TRUE),
      lyh[year == 1990][1],
      NA
    )
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    pop_avg_growth_20yr = ((midyear_population /
      lag(midyear_population, n = 20))^(1 / 20) -
      1) *
      100,
    # Handle cases where all values are NA
    dlyhmin = ifelse(all(is.na(dlyh)), NA, min(dlyh, na.rm = TRUE)),
    dlyhmax = ifelse(all(is.na(dlyh)), NA, max(dlyh, na.rm = TRUE))
  ) %>%
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
    year == opt.year & grouped_country.x != 1
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
# Decomposition 2010-2025, Europe
stacked_plot <- decomp_plot %>%
  ggplot(aes(
    x = reorder(country, contribution),
    y = contribution,
    fill = component
  )) +
  geom_col() +
  coord_flip() + # Makes country names easier to read
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    title = "GDP Growth Decomposition, Europe 2010-2025",
    subtitle = "Cumulative contributions by component",
    x = "Country",
    y = "Contribution to GDP Growth (%)",
    fill = "Component"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_blank()
  )
stacked_plot
ggsave(
  here::here('figures', "stacked_plot.png"),
  width = 5,
  height = 4,
  dpi = 300
)

#===========================================

# Productivity growth and Population Growth

#============================================

# PLOT
plot.labor_productivity_popgrowth <- dat |>
  filter(Europe.x == 1 & year == 2025) |>
  ggplot(aes(x = popg_0025, y = labor_productivity_change_2000_2025)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  geom_text_repel(aes(label = country)) +
  geom_vline(xintercept = 0.0, lty = 4) +
  geom_hline(yintercept = 0.0, lty = 4) +
  labs(
    title = "Labour productivity growth and Population growth",
    subtitle = "Europe, 2000-25",
    x = "Population growth, 2000-25 (avg. %pa)",
    y = "Labour Productivity Growth (2000-25)"
  )
plot.labor_productivity_popgrowth
ggsave(
  here::here('figures', "plot.labor_productivity_popgrowth.png"),
  width = 5,
  height = 4,
  dpi = 300
)


#==============

# Regression

#==============
library(modelsummary)

# replace with 20y pop change

mod1 <- lm(
  dlyh ~ lyh1990 + dlpop,
  data = dat
)

broom::tidy(mod1)

modelsummary(
  mod1,
  title = "Labor Productivity Growth Regression",
  coef_rename = c(
    "lyh1990" = "Log productivity 1990",
    "dlpop" = "Log population change"
  ),
  statistic = c("t = {statistic}", "p = {p.value}"),
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)
