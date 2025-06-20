# 01_create_data.R
# Analysis of Conf Board cross-country Labour Productivity data
 

rm(list=ls())
x = c('tidyverse', 'readxl', 'broom', 'modelr', 
      'ggtext', 'ggrepel', 'ggsci', 'janitor', 'zoo', 'showtext')
invisible(lapply(x, library, character.only = TRUE))

# 1. Plot Settings and Data cleaning functions ----
source(here::here('functions', 'functions.R'))
showtext_auto() # Automatically use system fonts

# 2. Get Data; TED version saved locally ----
tseries.file1 <- here::here('data', "TED_1_JULY20201.xlsx")
df1  <- read_excel(tseries.file1, sheet = "TCB_ADJUSTED", range = "A5:BW1985")

# 3. Tidy Data # df1: Growth rates; df2: TED data on Contributions ----
df1 <- clean_stage1(df1)

# Data Selection (SELECT Regions + Years) ----
df1 <- df1 %>%
  dplyr::filter(
    REGION %in% c("Western Europe", "North America", "Oceania") | ISO == "JPN",
    year >= 1960
  )

# Data [df2] Adding GDP per capita and Growth Rates in 2019 ----
df2019 <- df1 %>%
  arrange(ISO, year) %>%
  group_by(ISO) %>%
  mutate(gdppc1960 = `Per Capita Income`[year==1960],
         gdppc2000 = `Per Capita Income`[year==2000],
         g19602019 = 100*((`Per Capita Income`[year==2019] / `Per Capita Income`[year==1960])^(1/(2019-1960)) - 1),
         g20002019 = 100*((`Per Capita Income`[year==2019] / `Per Capita Income`[year==2000])^(1/(2019-2000)) - 1)) %>%
  dplyr::filter(year==last(year)) 

# Data [df3] for Growth in Output per Hour Worked and Employed Person ----
df3 <- df1 %>%
  group_by(year) %>%
  mutate(dlyhMin = min(`Output per Hour Worked growth`, na.rm = TRUE),
         dlyhMax = max(`Output per Hour Worked growth`, na.rm = TRUE),
         dlyWMin = min(`Output per Employed Person growth`, na.rm = TRUE),
         dlyWMax = max(`Output per Employed Person growth`, na.rm = TRUE),
         dlyhMean = mean(`Output per Hour Worked growth`, na.rm = TRUE ),
         dlyWMean = mean(`Output per Employed Person growth`, na.rm = TRUE)) %>%
  ungroup()

# 4. CONSTRUCT + SELECT: Data of 20y means, Y/L and Pop ----
df20 <- df1 %>%
  arrange(ISO, year) %>%
  group_by(ISO) %>%
  mutate(dyl20y = rollmean(x = `Output per Employed Person growth`, 20, fill = NA, align = "right"),
         dPop20y = rollmean(x = `Population growth`, 20, fill = NA, align = "right"), 
         ylinitial = lag(log(`Output per Employed Person`), 20)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(DpopMax= max(`Population growth`),
         DpopMin = min(`Population growth`)) %>%
  ungroup()

# DataFrames: By Country; By Country/Decade ----
byISO <- df20 %>%
  group_by(ISO) %>%
  summarise(across(c('Output per Employed Person growth',
                     'Employment growth',
                     'Population growth',
                     'Output per Employed Person'), mean, na.rm = TRUE)) 


byISOTime <- df20 %>%
  group_by(ISO, decade) %>%
  summarise(across(c('Output per Employed Person growth',
                     'Employment growth',
                     'Population growth',
                     'Output per Employed Person'), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ISO) %>%
  mutate(ylslow = ifelse(decade=='2010-19', `Output per Employed Person growth` - lag(`Output per Employed Person growth`,2), NA),
         dPopG = ifelse(decade=='2010-19', `Population growth` - lag(`Population growth`,2), NA))

# Nested List-Columns Cross-section Variation, By Year; REGRESSION ----
byYear <- df20 %>%
  dplyr::filter(ylinitial!="NA") %>%
  nest(data = -year) %>%
  mutate(
    fit = map(data, ~lm(dyl20y ~
                          dPop20y + ylinitial,
                        data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

byCountry <- df20 %>%
  dplyr::filter(ylinitial!="NA") %>%
  nest(data = -ISO) %>%
  mutate(
    fit = map(data, ~lm(dyl20y ~
                          dPop20y + ylinitial,
                        data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

# 5. Save DataFrames ----
saveRDS(df1,    here::here('data', 'df1.rds')) # basic
saveRDS(df2019, here::here('data', 'df2019.rds')) # 2019 cross-section
saveRDS(df20,   here::here('data', 'df20.rds')) # 20-year means
saveRDS(byISO,  here::here('data', 'byISO.rds')) # by ISO country
saveRDS(byISOTime, here::here('data', 'byISOTime.rds')) # by ISO country/decade
saveRDS(byYear, here::here('data', 'byYear.rds')) # by Year regression (list columns)
saveRDS(byCountry, here::here('data', 'byCountry.rds')) # by Country regression (list column)

print("Conference Board Dataframes created and saved")
