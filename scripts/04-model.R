#### Preamble ####
# Purpose: Models whether we guess right based off of our confidence
# Author: Sachin Chhikara
# Date: 26 March 2024 
# Contact: sachin.chhikarar@utoronto.ca
# License: MIT
# Pre-requisites: 02-data_cleaning.R

#### Workspace setup ####
library(arrow)
library(tidyverse)
library(rstanarm)
library(modelsummary)

#### Read data ####
analysis_data <- read_parquet(file = "data/analysis_data/analysis_data.parquet")


### Model data ####
first_model <-
  stan_glm( 
    formula = FinalExamination ~ Group,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )

modelsummary(first_model)

#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)