# Total Economy Database: Cross-country comparisons of Productivity
# ABenito

rm(list=ls())
library(tidyverse)
library(magrittr)
library(readxl)
library(lme4)
library(modelr)
library(gghighlight)
library(ggtext)
library(ggrepel)
library(ggthemes)
library(ggsci)
library(showtext)

# Settings: Display ----
font_add_google("Roboto Condensed", "Roboto Condensed")
theme_set(theme_bw(base_size = 15, base_family = "Roboto Condensed"))
theme_bw <- theme_update(panel.background = element_rect(fill = "white"),
                         panel.grid.major = element_line(linewidth = .5),
                         panel.grid.minor = element_blank(),
                         axis.text = element_text(color = "dodgerblue"),
                         legend.position = c(.45, .2),
                         legend.background = element_rect(fill = "transparent"))

# GetData; TED saved locally ----
tseries.file1 <- "~/Research/Data/03_World/07_TED/TED_1_JULY20201.xlsx"
tseries.file2 <- "~/Research/Data/03_World/07_TED/TED_2_JULY20201.xlsx"
df1  <- read_excel(tseries.file1, sheet = "TCB_ADJUSTED", range = "A5:BW1985")
df2  <- read_excel(tseries.file2, sheet = "TCB_ADJUSTED", range = "A5:AJ2117")

# Tidy Data # df1: TED data for Growth rates; df2: TED data on Contributions ----
df1 <- df1 %>%
  pivot_longer(
    cols = -c(COUNTRY, REGION, ISO, INDICATOR, MEASURE),
    names_to = "year",
    values_to = "value",
    values_drop_na = FALSE
  ) %>%
  mutate(year = as.numeric(year))

df2 <- df2 %>%
  pivot_longer(
    cols = -c(COUNTRY, REGION, ISO, INDICATOR, MEASURE),
    names_to = "year",
    values_to = "value",
    values_drop_na = FALSE
  ) %>%
  mutate(year = as.numeric(year))

# Select Sub-sample: n=24 ----
df1X <- df1 %>%
  filter(INDICATOR == "Output per Hour Worked") %>%
  arrange(COUNTRY, year) %>%
  filter(REGION=="Western Europe"|REGION=="North America"|REGION=="Oceania")

# Select Sub-sample: n=24, incl Pop variable
df1XX <- df1 %>%
  filter(INDICATOR == "Output per Hour Worked" | INDICATOR=="Employment") %>%
  arrange(COUNTRY, year) %>%
  filter(REGION=="Western Europe"|REGION=="North America"|REGION=="Oceania")


# FIGURES in Paper ----
# ==================
# Fig9a: Labour Productivity Line-plots by Country [Annex] ---- 
ggplot(df1X, aes(x=year, y = value, color = COUNTRY)) + 
  geom_line() +
  labs(x = "Year", y = "Output per Hour Worked, 2019 US $000") + 
  gghighlight(COUNTRY=="United Kingdom"|COUNTRY=="United States"|COUNTRY=="Germany"|COUNTRY=="France", use_direct_label = FALSE) + 
  scale_color_futurama("planetexpress") +
  guides(colour = guide_legend((title = ""))) +
  xlim(1950, 2020) +
  theme(legend.position = c(0.35,0.85)) 
#  labs(caption = "Source: The Conference Board. N=24 countries in W. Europe, N. America and Oceania")
ggsave("./docs/images/Fig9a.pdf", width = 5, height = 4, device = cairo_pdf)

# Pivot, with Pop variable; t-series of cross-sections
df1XXwide <- df1XX %>%
  select(!MEASURE) %>%
  pivot_wider(names_from = INDICATOR, values_from = c(value)) %>%
  rename(yl = 6) %>%
  mutate(dyl_201906 = ifelse(year==2019, 100*(yl/(lag(yl,13))-1), NA),
         dyl_201914 = ifelse(year==2019, 100*(yl/(lag(yl,5))-1), NA),
         dlEmp = 100*(log(Employment) - lag(log(Employment),13)),
         dlEmp_2019 = ifelse(year==2019, dlEmp, NA),
         yl_US05 = ifelse(ISO=='USA' & year==2005, yl, NA),
         yl_05 = ifelse(year==2005, yl, NA))

# df1Xa: Country-averages----
df1X <- df1X %>%
  mutate(dyl_201906 = ifelse(year==2019, 100*(value/(lag(value,13))-1), NA),
         dyl_201914 = ifelse(year==2019, 100*(value/(lag(value,5))-1), NA),
         yl_US05 = ifelse(ISO=='USA' & year==2005, value, NA),
         yl_05 = ifelse(year==2005, value, NA))

df1Xa <- df1X %>%
  group_by(COUNTRY) %>%
  summarise(dyl_201906 = mean(dyl_201906, na.rm = TRUE), 
            dyl_201914 = mean(dyl_201914, na.rm = TRUE), 
            yl_05 = mean(yl_05, na.rm = TRUE),
            yl_US05 = mean(yl_US05, na.rm = TRUE))

df1Xa <- df1Xa %>%    
  summarise(dyl_201906 = dyl_201906,
            dyl_201914 = dyl_201914,
            yl_US05 = mean(yl_US05, na.rm = TRUE),
            yl_05vUS = 100*(yl_05 / yl_US05),
            COUNTRY = COUNTRY)

# df1Xb: Country-averages incl Pop variable
df1Xb <- df1XXwide %>%
  group_by(COUNTRY) %>%
  summarise(dyl_201906 = mean(dyl_201906, na.rm = TRUE), 
            dyl_201914 = mean(dyl_201914, na.rm = TRUE), 
            yl_05 = mean(yl_05, na.rm = TRUE),
            yl_US05 = mean(yl_US05, na.rm = TRUE),
            dlEmp_2019 = mean(dlEmp_2019, na.rm = TRUE)) %>%
  ungroup() 

df1Xb <- df1Xb %>%
  mutate(yl_US05 = mean(yl_US05, na.rm = TRUE),
         yl_05vUS = 100 * yl_05 / yl_US05) 

# Chart: Ordered Bar of Cross-Country Growth in Productivity 2008-19 ----
df1Xa <- df1Xa[order(df1Xa$dyl_201906), ]
df1Xa$COUNTRY <- factor(df1Xa$COUNTRY, levels = df1Xa$COUNTRY) 

ggplot(df1Xa, aes(x=`COUNTRY`, y=dyl_201906)) + 
  geom_bar(stat='identity', width=.5)  +
  labs(subtitle="Growth in Output per hour worked 2006-19", title= "") + 
  xlab("") + ylab("Growth in output per hour, %") +
  scale_fill_jco() +
  coord_flip()

# Drop Greece and Ireland as Outliers
df1XbChart <- df1Xb %>%
  filter(!COUNTRY %in% c('Greece', 'Ireland'))

# Basic Convergence Chart
ggplot(df1Xb, aes(x=yl_05vUS, y=dyl_201906, label = COUNTRY)) +
  geom_point() + geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(subtitle="" , title= "") + 
  xlab("Labour productivity relative to the US, 2005") + ylab("Growth in productivity, 2006-19 (%)") 
ggsave("./docs/images/Fig9b.pdf", width = 5, height = 4, device = cairo_pdf)

model1 <- lm(data=df1XbChart, formula= dyl_201906 ~ yl_05vUS)
summary(model1)
df1XbChart$ResidMod1 <- residuals(model1)

# Take Residual and Plot against Pop growth
model2 <- lm(data=df1XbChart, formula= ResidMod1 ~ dlEmp_2019)
summary(model2)
df1XbChart$ResidMod2 <- residuals(model2)

ggplot(df1XbChart, aes(x=dlEmp_2019, y=ResidMod1, label = COUNTRY)) +
  geom_point() + geom_text_repel() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(subtitle="" , title= "") + 
  xlab("Population growth, 2006-19") + ylab("Residual from Productivity Convergence, 2008-19 (%)") 
ggsave("./docs/images/Fig9c.pdf", width = 5, height = 4, device = cairo_pdf)


# df1Xb <- df1Xb %>%
#   filter(!COUNTRY==c('United States')) 

model3 <- lm(data=df1Xb, formula= ResidMod1 ~ dlEmp_2019) 
summary(model3)


# Convergence Residual against Pop growth since 2008
ggplot(df1Xb, aes(x=dlEmp_2019, ResidMod1, label = COUNTRY)) + 
  geom_point() + geom_text_repel()  + 
  labs(subtitle="", title= "") + 
  xlab("Population growth (2008-19, %)") + ylab("Residual from Productivity Convergence Regression") 

model2 <- lm(data=df1Xb, formula= ResidMod1 ~ dlPop_2019)
summary(model2)
  
    
# Long Format: Ordered Cross-country Bar of Productivity Growth since 2008 or 2014 ----
dfChart2 <- gather(df1Xa, period, value, dyl_201906:dyl_201914) %>%
  mutate(a = ifelse(COUNTRY=='United Kingdom', "sienna", "black"))

# Chart: Bars for y/l 2008-19 and 2014-19
ggplot(dfChart2, aes(x=`COUNTRY`, value, fill = period)) + 
  geom_bar(stat='identity', position = 'dodge', width=0.9)  +
  labs(subtitle="", title= "") + 
  xlab("") + ylab("Growth in output per hour, %") +
  guides(fill = guide_legend((title = ""))) +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_fill_jco(labels = c("2008-19", "2014-19")) +
  theme(legend.position = c(0.75, .2)) +
  coord_flip() 
#  labs(caption = "Source: The Conference Board. N=24 countries in W. Europe, N. America and Oceania")
ggsave("./docs/images/Fig9b.pdf", width = 5, height = 7, device = cairo_pdf)

# Chart cross-plot of 2008-19 Productivity Shortfall against Population growth
ggplot(df1Xa, aes(x=dlPop_2019, y = ResidMod1)) + 
  geom_point()  +
  labs(subtitle="", title= "") + 
  xlab("Population growth, 2008-19") + ylab("Growth in output per hour, %") +
  guides(fill = guide_legend((title = ""))) +
  theme() 
#  labs(caption = "Source: The Conference Board. N=24 countries in W. Europe, N. America and Oceania")

# Chart of Contributions to GDP Growth - UK [Annex] ----
df2X <- df2 %>%
  filter(ISO=="GBR") %>%
  filter(INDICATOR=="Labor Quantity Contribution"|INDICATOR=="Labor Quality Contribution"|INDICATOR=="ICT Capital Contribution"|INDICATOR=="Non-ICT Capital Contribution"|INDICATOR=="Total Factor Productivity") %>%
  arrange(INDICATOR)

g <- ggplot(df2X)
g + geom_bar(aes(x=year, y=value, fill=INDICATOR), 
             stat="identity") +
  xlab("date") + ylab("GDP Growth") +
  guides(fill = guide_legend((title = ""))) +
  scale_fill_jco(
    labels = c("ICT Capital", "Labour Quality", "Labour Quantity", "Non-ICT Capital", "TFP")) +
  theme(legend.position = "bottom")
ggsave("Fig10a.pdf", width = 7, height = 6, device = cairo_pdf)

# Chart of Contributions to GDP Growth - US [Annex] ----
df2X <- df2 %>%
  filter(ISO=="USA") %>%
  filter(INDICATOR=="Labor Quantity Contribution"|INDICATOR=="Labor Quality Contribution"|INDICATOR=="ICT Capital Contribution"|INDICATOR=="Non-ICT Capital Contribution"|INDICATOR=="Total Factor Productivity") %>%
  arrange(INDICATOR)

g <- ggplot(df2X)
g + geom_bar(aes(x=year, y=value, fill=INDICATOR), 
             stat="identity") +
  xlab("date") + ylab("GDP Growth") +
  guides(fill = guide_legend((title = ""))) +
  scale_fill_jco(
    labels = c("ICT Capital", "Labour Quality", "Labour Quantity", "Non-ICT Capital", "TFP")) +
  theme(legend.position = "bottom")
ggsave("./docs/images/Fig10b.pdf", width = 7, height = 6, device = cairo_pdf)

write_csv(df1, "./data/dataFrame7a.csv")
write_csv(df2, "./data/dataFrame7b.csv")

# All Countries ----

# Select Subsample
df1X <- df1 %>%
  filter(INDICATOR == "Output per Hour Worked") %>%
  arrange(COUNTRY, year) 

g1 <- ggplot(df1X, aes(x=year, y = value, color = COUNTRY)) + 
  geom_line() +
  scale_colour_viridis_d()  +
  labs(x = "Year", y = "Output per Hour Worked, 2019 US$") + 
  gghighlight(COUNTRY=="United Kingdom"|COUNTRY=="United States"|COUNTRY=="Germany"|COUNTRY=="France", use_direct_label = FALSE) + 
  guides(colour = guide_legend((title = ""))) +
  xlim(1950, 2020) +
  theme(legend.position = "bottom") +
  labs(caption = "Source: The Conference Board Total Economy Database. Sample is 24 countries in Western Europe, N. America and Oceania")
g1

