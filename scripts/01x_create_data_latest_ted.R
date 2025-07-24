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

# source functions
source(here::here('functions', 'functions.R'))

# 1. Plot Settings and Data cleaning functions ----
#source(here::here('functions', 'functions.R'))
showtext_auto() # Automatically use system fonts

# 2. Get Data; TED version saved locally ----
tseries.file1 <- here::here(
  'data/raw',
  "TED_output_labour_labourproductivity.xlsx"
)
tseries.file2 <- here::here('data/raw', "TED_growth_accounting_tfp.xlsx")

# Define Europe and Country Groups
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

groups <- c(
  "mature_economies",
  "middle_east_north_africa",
  "other_developing_asian_economies",
  "other_mature_economies",
  "russia_central_asia_and_southeast_europe",
  "russian_federation",
  "sub_saharan_africa",
  "emerging_markets_and_developing_economies"
)


# Clean TED data
df1 <- clean_ted_data(tseries.file1)
df2 <- clean_ted_data(tseries.file2)

dat <- left_join(df1, df2, by = c("country", "year"))

# save data
saveRDS(df1, "data/df1.rds")
saveRDS(df2, "data/df2.rds")
