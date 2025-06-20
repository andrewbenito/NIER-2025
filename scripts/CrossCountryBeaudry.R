# Total Economy Database: Cross-country comparisons of Productivity
# ABenito

# Packages ----
rm(list=ls())
x = c('tidyverse', 'broom', 'modelr', 'ggtext', 'ggrepel', 'ggsci',
      'janitor', 'zoo', 'GGally', 'showtext')
invisible(lapply(x, library, character.only = TRUE))

# Settings: Display ----
theme_set(theme_bw(base_size = 15, base_family = "Roboto Condensed"))
theme_bw <- theme_update(panel.background = element_rect(fill = "white"),
                         panel.grid.major = element_line(linewidth = .5),
                         panel.grid.minor = element_blank(),
                         axis.text = element_text(color = "dodgerblue"),
                         legend.position = c(.45, .2),
                         legend.background = element_rect(fill = "transparent"))

# GetData; TED saved locally ----
tseries.file1 <- "~/Research/Data/03_World/07_TED/TED_1_JULY20201.xlsx"
df1  <- read_excel(tseries.file1, sheet = "TCB_ADJUSTED", range = "A5:BW1985")

# Tidy Data # df1: TED data for Growth rates; df2: TED data on Contributions ----
df1 <- df1 %>%
  select(!MEASURE) %>%
  pivot_longer(
    cols = -c(COUNTRY, REGION, ISO, INDICATOR),
    names_to = "year",
    values_to = "value",
    values_drop_na = FALSE) %>%
  mutate(year = as.numeric(year)) 

# Decades ----
df1 <- df1 %>% pivot_wider(names_from = INDICATOR, values_from = value) %>%
  arrange(ISO, year) %>%
  mutate(decade = case_when(
    year %in% 1950:1959 ~ "1950-59", year %in% 1960:1969 ~ "1960-69",
    year %in% 1970:1979 ~ "1970-79", year %in% 1980:1989 ~ "1980-89",
    year %in% 1990:1999 ~ "1990-99", year %in% 2000:2009 ~ "2000-9",
    year %in% 2010:2019 ~ "2010-19")) 

# SELECT Regions + Years ----
df1 <- df1 %>%
  filter(REGION=="Western Europe"|REGION=="North America"|REGION=="Oceania"|ISO=="JPN") %>%
  filter(year>=1960)

# Beta-Convergence - 1960
df2 <- df1 %>%
  arrange(ISO, year) %>%
  group_by(ISO) %>%
  mutate(gdppc1960 = `Per Capita Income`[year==1960],
         gdppc2000 = `Per Capita Income`[year==2000],
         g19602019 = 100*((`Per Capita Income`[year==2019] / `Per Capita Income`[year==1960])^(1/(2019-1960)) - 1),
         g20002019 = 100*((`Per Capita Income`[year==2019] / `Per Capita Income`[year==2000])^(1/(2019-2000)) - 1)) %>%
  filter(year==last(year)) 

df3 <- df1 %>%
  group_by(year) %>%
  mutate(dlyhMin = min(`Output per Hour Worked growth`, na.rm = T),
         dlyhMax = max(`Output per Hour Worked growth`, na.rm = T),
         dlyWMin = min(`Output per Employed Person growth`, na.rm = T),
         dlyWMax = max(`Output per Employed Person growth`, na.rm = T),
         dlyhMean = mean(`Output per Hour Worked growth`, na.rm = T ),
         dlyWMean = mean(`Output per Employed Person growth`, na.rm = T)) %>%
  ungroup()

# CHART: From 1960 = convergence
ggplot(df2, aes(x = gdppc1960, y = g19602019)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  geom_text_repel(aes(label = ISO), hjust=-0.5)  +
  xlab( "GDP per capita, 1960") +
  ylab("Growth rate, 1960-2019 (annual rate, %)")

# CHART: From 2000
ggplot(df2, aes(x = gdppc2000, y = g20002019)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  geom_text_repel(aes(label = ISO), hjust=-0.5)  +
  xlab("GDP per capita, 2000") +
  ylab("Growth rate, 2000-2019 (annual rate, %)")

# Ribbon Chart, UK and Max and Min within AEs
ggplot(df3, aes(x=year)) +
  geom_ribbon(aes(ymin=dlyhMin, ymax= dlyhMax),
              alpha = 0.2,
              linetype = 1,
              fill = "red") + 
  geom_line(data = subset(df3, ISO %in% c("GBR")), aes(y = `Output per Hour Worked growth`)) +
#  stat_summary(y = `Output per Hour Worked growth`, fun = "mean", geom="line", colour="green") +
  scale_color_jco() +
  xlab("Year") + ylab("Growth in Output per Hour Worked, %pa") +
  labs(caption = "Sources: Conference Board, NIESR") + theme(plot.caption = element_text(hjust = 0)) 
ggsave("./docs/images/Fig1bX.pdf", width = 5, height = 4, device = cairo_pdf)

# add average line
ggplot(df3, aes(x=year)) +
  geom_ribbon(aes(ymin=dlyhMin, ymax= dlyhMax),
              alpha = 0.2,
              linetype = 1,
              fill = "red") + 
  geom_line(data = subset(df3, ISO %in% c("GBR")), aes(y = `Output per Hour Worked growth`)) +
#  stat_summary(y = `Output per Hour Worked growth`, fun = "mean", geom="line", colour="green") +
  xlab("Year") + ylab("Growth in Output per Hour Worked, %pa") + 
  ggtitle("An international productivity slowdown")


# CONSTRUCT + SELECT: 20y means, Y/L and Pop ----
df1 <- df1 %>%
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
byISO <- df1 %>%
  group_by(ISO) %>%
  summarise(across(c('Output per Employed Person growth',
                     'Employment growth',
                     'Population growth',
                     'Output per Employed Person'), mean, na.rm = TRUE)) 

byISOTime <- df1 %>%
  group_by(ISO, decade) %>%
  summarise(across(c('Output per Employed Person growth',
                     'Employment growth',
                     'Population growth',
                     'Output per Employed Person'), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ISO) %>%
  mutate(ylslow = ifelse(decade=='2010-19', `Output per Employed Person growth` - lag(`Output per Employed Person growth`,2), NA),
         dPopG = ifelse(decade=='2010-19', `Population growth` - lag(`Population growth`,2), NA))

# Figures ----
  
# Population growth and Labour Productivity
ggplot(byISO, aes(x=`Population growth`, y = `Output per Employed Person growth`)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) + 
  geom_text_repel(aes(label = ISO), hjust=-0.5) + 
  xlab("Population growth (avg annual rate, %)") +
  ylab("Growth in Output per Person Employed (avg annual rate, %)")
ggsave("./docs/images/FIG12a.pdf", width = 5, height = 4, device = cairo_pdf)

mod1 <- lm(`Output per Employed Person growth` ~ `Population growth`, data = byISO)
summary(mod1)

# Population growth by Sub-period
ggplot(byISOTime, aes(x=`Population growth`, y = `Output per Employed Person growth`)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) + 
  facet_wrap(~decade) + 
  geom_text_repel(aes(label = ifelse(ISO=="GBR", ISO, '')), hjust=-0.5, max.overlaps = Inf) + 
  xlab("Population growth (avg annual rate, %)") +
  ylab("Growth in Output per Person Employed (avg annual rate, %)")
ggsave("./docs/images/FIG12b.pdf", width = 5, height = 4, device = cairo_pdf)

# UK Population growth over time
df1 %>%
  filter(ISO=='GBR') %>%
  ggplot(aes(x = year, y = `Population growth`)) + 
  geom_line() + 
  ylab("Population growth, %pa") + 
  geom_hline(yintercept = 0.0, lty = 4) +
  labs(subtitle = "Population growth, UK")

# Population growth, by Country
ggplot(subset(df1, ISO != 'GBR'), aes(x = year, y = `Population growth`)) + 
  geom_line(aes(group = ISO)) +
  geom_line(data = subset(df1, ISO == 'GBR'), 
            size = 1, color = 'red') +
  scale_color_manual(values = 'gray') +
  ylab("Population growth, %pa") + 
  labs(subtitle = "Population growth, by country")

# Cross-plot Change in Pop growth and Productivity growth
byISOTime %>%
  ggplot(aes(x=dPopG, y = `Output per Employed Person growth`)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) + 
  geom_text_repel(aes(label = ifelse(ISO=="GBR", ISO, '')), hjust=-0.5, max.overlaps = Inf) + 
  xlab("Change in Population growth in 2010s pp)") +
  ylab("Growth in Output per Person Employed (avg annual rate, %)")

# Nested List-Columns Cross-section Variation, By Year; REGRESSION ----
byYear <- df1 %>%
  filter(ylinitial!="NA") %>%
  nest(data = -year) %>%
  mutate(
    fit = map(data, ~lm(dyl20y ~
                        dPop20y + ylinitial,
                        data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
    )

# if UK
df1 |>
  mod2 = lm(`Output per Employed Person growth` ~ `Population growth`, data = .x)



# Plot Coefficients ---- 
# Population Growth
byYear %>%
  unnest(tidied) %>%
  filter(term=='dPop20y') %>%
  mutate(conf.low_90 = estimate - 1.645*std.error,
         conf.high_90 = estimate + 1.645*std.error) %>%
  ggplot(aes(x=year, y = estimate)) +
  geom_point(aes(x = year, y = estimate, color = "Point Estimate")) +
  geom_linerange(aes( x = year, 
                      ymin=conf.low_90,
                      ymax=conf.high_90),
                 lwd = 1, color = "dodgerblue") +
  geom_hline(yintercept=0, linetype = "dashed") +
  xlab("Final year (20-year sample)") + ylab("Coefficient on Pop growth") + 
  labs(subtitle = "Point estimates and 90% confidence interval") +
  theme(legend.title = element_blank(), legend.position = c(0.8,0.2)) 
ggsave("./docs/images/FIG13a.pdf", width = 5, height = 4, device = cairo_pdf)

# Convergence Estimate 
byYear %>%
  unnest(tidied) %>%
  filter(term=='ylinitial') %>%
  mutate(conf.low_90 = estimate - 1.645*std.error,
         conf.high_90 = estimate + 1.645*std.error) %>%
  ggplot(aes(x=year, y = estimate)) +
  geom_point(aes(x = year, y = estimate, color = "Point Estimate")) +
  geom_linerange(aes( x = year, 
                      ymin=conf.low_90,
                      ymax=conf.high_90),
                 lwd = 1, color = "dodgerblue") +
  xlab("Final year (20-year sample)") + ylab("Convergence estimate (%pa)") + 
  labs(subtitle = "Point estimates and 90% confidence interval") +
  theme(legend.title = element_blank(), legend.position = c(0.8,0.2)) 
ggsave("./docs/images/FIG13b.pdf", width = 5, height = 4, device = cairo_pdf)




