# functions for cross-country data analysis using Conf Board data

# Settings: Display ----
theme_set(theme_bw(base_size = 13, base_family = "Roboto Condensed"))
theme_bw <- theme_update(
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(linewidth = .5),
  panel.grid.minor = element_blank(),
  axis.text = element_text(color = "dodgerblue"),
  legend.position = c(.45, .2),
  legend.background = element_rect(fill = "transparent")
)

# Functions for Data Cleaning

clean_stage1 <- function(df) {
  df <- df %>%
    select(!MEASURE) %>%
    pivot_longer(
      cols = -c(COUNTRY, REGION, ISO, INDICATOR),
      names_to = "year",
      values_to = "value",
      values_drop_na = FALSE
    ) %>%
    mutate(year = as.numeric(year)) %>%
    pivot_wider(names_from = INDICATOR, values_from = value) %>%
    arrange(ISO, year) %>%
    mutate(
      decade = case_when(
        year %in% 1950:1959 ~ "1950-59",
        year %in% 1960:1969 ~ "1960-69",
        year %in% 1970:1979 ~ "1970-79",
        year %in% 1980:1989 ~ "1980-89",
        year %in% 1990:1999 ~ "1990-99",
        year %in% 2000:2009 ~ "2000-9",
        year %in% 2010:2019 ~ "2010-19",
        TRUE ~ NA_character_
      )
    )

  return(df)
}


# Function to safely convert to numeric
safe_numeric <- function(x) {
  # Try to convert, return original if it fails
  tryCatch(as.numeric(x), warning = function(w) x)
}

# function to clean TED data
clean_ted_data <- function(file_path) {
  # Load and prepare
  df <- read_excel(file_path)
  colnames(df) <- df[1, ]
  names(df)[1] <- "year"
  df <- df |> clean_names() |> slice(-1:-2, -4:-6)

  # Build regex from country names
  countries <- janitor::make_clean_names(as.character(df[1, ]))
  countries <- unique(sub("_[0-9]+$", "", countries))
  country_regex <- paste(countries, collapse = "|")

  # Long format
  df_long <- pivot_longer(
    df,
    cols = -year,
    names_to = c("country", "variable"),
    names_pattern = paste0("^(", country_regex, ")_(.*)$")
  ) |>
    filter(!is.na(year) & !is.na(value))

  # Wide format, clean
  df_clean <- df_long |>
    pivot_wider(names_from = variable, values_from = value) |>
    arrange(country, year) |>
    select(-contains("alternative")) |>
    mutate(
      Europe = country %in% europe,
      grouped_country = country %in% groups
    ) |>
    mutate(across(everything(), safe_numeric)) |>
    mutate(across(
      everything(),
      ~ if (is.character(.x) && any(str_detect(.x, "%"), na.rm = TRUE)) {
        as.numeric(str_remove(.x, "%"))
      } else {
        .x
      }
    ))

  arrange(df_clean, country, year)

  return(df_clean)
}
