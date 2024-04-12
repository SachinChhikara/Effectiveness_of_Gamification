#### Preamble ####
# Purpose: Models the assessment given to the student against the group they were in
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

### Model data ####
Word_model <-
  stan_glm( 
    formula = WordProcPosttest ~ Group,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 849
  )

modelsummary(Word_model)

Spreadsheet_model <-
  stan_glm( 
    formula = SpreadsheetsPosttest ~ Group,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 849
  )

modelsummary(Spreadsheet_model)

Database_model <-
  stan_glm( 
    formula = DatabasesPosttest ~ Group,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 849
  )

modelsummary(Database_model)


FinalExam_model <-
  stan_glm( 
    formula = FinalExamination ~ Group,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )

modelsummary(FinalExam_model)

#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)