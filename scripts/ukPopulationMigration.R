
# Population - Growth and composition
# Long annual time-series of population

library(tidyverse)
library(sysfonts)
library(ggsci)
    
births <- read.csv("./data/birthsONS.csv") %>%
#urlbirths <-  "https://www.ons.gov.uk/visualisations/dvc775/fig2/Line/datadownload.csv"  
  rename(year = 1, births = 2) %>%
  type_convert() %>%
  add_case(year=2019, births = 712680)
deaths <- read.csv("./data/deathsONS.csv") %>%
#urldeaths <- "https://www.ons.gov.uk/generator?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/articles/ourpopulationwherearewehowdidwegetherewherearewegoing/2020-03-27/17ffe146&format=csv"  
  slice(6:n()) %>%
  .[-1,] %>%
  rename(year = 1, deaths = 2) %>%
  type.convert() %>%
  add_case(year=2019, deaths = 604707)

df <- full_join(births, deaths) %>%
  mutate(births = births/1000, deaths = deaths/1000, 
         naturalD = births - deaths)

# Settings: Display ----
font_add_google("Roboto Condensed", "Roboto Condensed")
theme_set(theme_bw(base_size = 15, base_family = "Roboto Condensed"))
theme_bw <- theme_update(panel.background = element_rect(fill = "white"),
                         panel.grid.major = element_line(size = .5),
                         panel.grid.minor = element_blank(),
                         axis.text = element_text(color = "dodgerblue"))


# Births, Deaths, 'Natural' Change
g1 <- ggplot(df) + 
  geom_line(aes(x=year, y = births, color = "births" )) + 
  geom_line(aes(x=year, y = deaths, color = "deaths" )) +
  geom_area(aes(year, naturalD, fill = 'births less deaths')) +
  labs(x = "Year", y = "000s",
       caption = "Sources: ONS, NIESR") + theme(plot.caption = element_text(hjust = 0)) + 
  guides(colour = guide_legend((title = ""))) +
  theme(legend.title = element_blank()) + 
  scale_colour_jco() +
  theme(legend.position = "bottom") +
  xlim(1900, 2020)
g1
ggsave("./docs/images/Fig8a.pdf", width = 5, height = 4, device = cairo_pdf)


# Use patchwork to add plot of Net migration; alongside births less deaths

# p1 + p2 + 
#   plot_layout(widths = c(2, 0), heights = unit(c(5, 1), c('cm', 'null')))
