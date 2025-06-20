# functions for cross-country data analysis using Conf Board data



# Settings: Display ----
theme_set(theme_bw(base_size = 15, base_family = "Roboto Condensed"))
theme_bw <- theme_update(panel.background = element_rect(fill = "white"),
                         panel.grid.major = element_line(linewidth = .5),
                         panel.grid.minor = element_blank(),
                         axis.text = element_text(color = "dodgerblue"),
                         legend.position = c(.45, .2),
                         legend.background = element_rect(fill = "transparent"))

# Functions for Data Cleaning

clean_stage1 <- function(df) {
  df <- df %>%
  select(!MEASURE) %>%
  pivot_longer(
    cols = -c(COUNTRY, REGION, ISO, INDICATOR),
    names_to = "year",
    values_to = "value",
    values_drop_na = FALSE) %>%
  mutate(year = as.numeric(year)) %>% 
  pivot_wider(names_from = INDICATOR, values_from = value) %>%
  arrange(ISO, year) %>%
  mutate(decade = case_when(
    year %in% 1950:1959 ~ "1950-59", year %in% 1960:1969 ~ "1960-69",
    year %in% 1970:1979 ~ "1970-79", year %in% 1980:1989 ~ "1980-89",
    year %in% 1990:1999 ~ "1990-99", year %in% 2000:2009 ~ "2000-9",
    year %in% 2010:2019 ~ "2010-19",
    TRUE ~ NA_character_))  
  
  return(df)
  
}


