#### Preamble ####
# Purpose: Cleans the raw data
# Author: Sachin Chhikara
# Date: 8 April 2024 
# Contact: sachin.chhikarar@utoronto.ca
# License: MIT

#### Workspace setup ####
library(arrow)
library(tidyverse)

#### Clean data ####
raw_data <- read_csv("data/raw_data/1-s2.0-S2352340917300173-mmc2.csv")


cleaned_data <-
  raw_data |>
  select(-Age) |>
  mutate(Group = case_when(
    Group == 0 ~ "Control group",
    Group == 1 ~ "Educational game",
    Group == 2 ~ "Gamification plugin",
    Group == 3 ~ "Social networking website",
    Group == 4 ~ "Social gamified networking website"
  ),
  Gender = case_when(Gender == "M" ~ "Male", Gender == "F" ~ "Female")
  )



#### Save data ####
write_parquet(cleaned_data, "data/analysis_data/analysis_data.parquet")