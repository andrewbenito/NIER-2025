# 01b_create_data_update.R from updated version of TED data
# works on raw data downloaded from https://data-central.conference-board.org/
# July 2025
# Analysis of Conf Board cross-country Labour Productivity data

# Packages ----
rm(list = ls())
x = c(
  'tidyverse',
  'readxl',
  'broom',
  'modelr',
  'gt',
  'gtExtras',
  'ggtext',
  'ggrepel',
  'ggsci',
  'janitor',
  'zoo',
  'showtext'
)
invisible(lapply(x, library, character.only = TRUE))

# 1. Plot Settings and Data cleaning functions ----
#source(here::here('functions', 'functions.R'))
showtext_auto() # Automatically use system fonts

# 2. Get Data; TED version saved locally ----
tseries.file1 <- here::here(
  'data/raw',
  "TED_output_labour_labourproductivity.xlsx"
)
tseries.file2 <- here::here('data/raw', "TED_growth_accounting_tfp.xlsx")

# Define Europe
europe = sort(c(
  "poland",
  "romania",
  "latvia",
  "estonia",
  "bulgaria",
  "lithuania",
  "serbia",
  "slovak_republic",
  "albania",
  "malta",
  "czech_republic",
  "bosnia_herzegovina",
  "spain",
  "croatia",
  "slovenia",
  "ireland",
  "denmark",
  "switzerland",
  "sweden",
  "hungary",
  "portugal",
  "cyprus",
  "macedonia",
  "italy",
  "greece",
  "germany",
  "france",
  "austria",
  "belgium",
  "united_kingdom",
  "norway",
  "finland",
  "netherlands",
  "luxembourg"
))

# Clean TED data
df1 <- clean_ted_data(tseries.file1)
df2 <- clean_ted_data(tseries.file2)

saveRDS(df1, "data/df1.rds")
saveRDS(df2, "data/df2.rds")


View(df2)
# 3. Tidy Data # df1: Growth rates; df2: TED data on Contributions ----
# clean names
df1 <- read_excel(tseries.file1)
df2 <- read_excel(tseries.file2)

colnames(df1) <- df1[1, ]
names(df1)[1] <- "year"
df1 <- df1 |> clean_names() |> slice(-1:-2, -4:-6)

# a complex pivot_longer for combined country and variable name, following a complex regex
countries <- janitor::make_clean_names(as.character(df1[1, ]))
countries <- unique(sub("_[0-9]+$", "", countries)) # drop 'alternative'
#countries <- countries[!grepl("alternative", countries)]
country_regex <- paste(countries, collapse = "|")

df_long <- pivot_longer(
  df1,
  cols = -year,
  names_to = c("country", "variable"),
  names_pattern = paste0("^(", country_regex, ")_(.*)$")
) |>
  filter(!is.na(year) & !is.na(value))

df1 <- df_long |>
  pivot_wider(names_from = variable, values_from = value) |>
  arrange(country, year) |>
  select(-contains("alternative"))

df1 <- df1 |>
  mutate(Europe = country %in% europe) |>
  mutate(across(everything(), safe_numeric))

# SAVE FILE
saveRDS(df1, file = "data/df1.rds")
