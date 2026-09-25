packages <- c("here", "janitor", "tidyverse", "sf", "terra", "tmap", "spData", "spDataLarge", "geodata", "kableExtra", "viridisLite")
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

library(here)
library(janitor)
library(tidyverse)
library(sf)
library(kableExtra)

gdw_df <- read_csv(here("Weekly_Labs/Week 0/data", "gdw.csv")) |>
  clean_names() # Convert variable names to lower snake case

head(gdw_df, n = 10) |> 
  kable()

tail(gdw_df, n = 10) |> 
  kable()

dim(gdw_df)
nrow(gdw_df)
ncol(gdw_df)

names(gdw_df)

country_df <- gdw_df[,"country"]
country_vec <- gdw_df[["country"]]

gdw_df |> 
  group_by(dam_type) |> 
  summarise(count = n()) |> 
  ungroup()

sub_dam <- gdw_df |> 
  filter(dam_type == "Dam")

gdw_df <- gdw_df |> 
  arrange(year_dam)

gdw_df |>
  group_by(country) |>
  summarize(mean_dam_hgt_m = mean(dam_hgt_m, na.rm = TRUE)) |>
  ungroup() |> 
  ggplot(aes(x = country, y = mean_dam_hgt_m)) +
    geom_bar(stat = "identity") +
    labs(x = "Country",
         y = "Average height of dam/barrier in meters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data = gdw_df,
      aes(x = cap_mcm, y = dam_hgt_m)) +
  geom_point() +
  labs(x = "Storage capacity of reservoir in million cubic meters",
       y = "Height of dam/barrier in meters") +
  theme_minimal()

head(gdw_df$shape, n = 3)

class(gdw_df$shape)

gdw_st <- st_read(here("Weekly_Labs/Week 0/data", "gdw.gdb")) |>
  clean_names() # Convert variable names to lower snake case

gdw_sf <- read_sf(here("Weekly_Labs/Week 0/data", "gdw.gdb")) |>
  clean_names() # Convert variable names to lower snake case
