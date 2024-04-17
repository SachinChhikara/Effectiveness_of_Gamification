#### Preamble ####
# Purpose: Model created for inference about the final examination results
# Author: Sachin Chhikara
# Date: 12 April 2024 
# Contact: sachin.chhikarar@utoronto.ca
# License: MIT

#### Workspace setup ####
library(arrow)
library(tidyverse)
library(rstanarm)
library(modelsummary)

#### Read data ####
analysis_data <- read_parquet(file = "data/analysis_data/analysis_data.parquet")

FinalExam_model <-
  stan_glm( 
    formula = FinalExamination ~ Group * Gender,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )

#### Save model ####
saveRDS(
  FinalExam_model,
  file = "models/final_exam.rds"
)