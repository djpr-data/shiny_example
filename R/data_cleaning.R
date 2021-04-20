# The purposes of this script is to provide a minimal example of some
# functions used in the construction of a Shiny app

library(tidyverse)
library(readabs)

# Load data ----

lfs_raw <- read_abs("6202.0", "5")

# Filter data ----

lfs <- lfs_raw %>%
  filter(series_type == "Seasonally Adjusted",
         !is.na(value)) %>%
  separate_series(column_names = c("series_desc", "sex")) %>%
  select(series_desc, sex, date, value)

unemp <- lfs %>%
  filter(series_desc == "Unemployment rate")

save(unemp, file = file.path("data", "unemp.Rda"))

# Visualise data ----

unemp %>%
  ggplot(aes(x = date, y = value, colour = sex)) +
  geom_line() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.position = "top")

# Function to visualise data -----

viz_ur <- function(df,
                   min_date = min(df$date),
                   sex = c("Persons", "Males", "Females")) {

  df <- df %>%
    filter(date >= min_date,
           .data$sex %in% .env$sex)

  df %>%
    ggplot(aes(x = date, y = value, colour = sex)) +
    geom_line() +
    theme_minimal() +
    scale_x_date(date_labels = "%b\n%Y") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          legend.position = "top")

}
