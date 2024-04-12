#### Preamble ####
# Purpose: Models whether we guess right based off of our confidence
# Author: Sachin Chhikara
# Date: 11 April 2024 
# Contact: sachin.chhikarar@utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(arrow)
library(knitr)
library(kableExtra)

#### Load data ####
analysis_data <- read_parquet(file = "data/analysis_data/analysis_data.parquet")

#number of people in each group
value_counts <- table(analysis_data$Group)


### Helper Functions ###
summarize_column_stats <- function(data, column_name) {
  stats <- data %>%
    group_by(Group) %>%
    summarise(
      mean = round(mean({{column_name}}), 2),
      sd = round(sd({{column_name}}), 2),
      se = round(sd({{column_name}})/sqrt(n()), 2)
    )
  
  return(stats)
}


#### Tables ####

# Table 2
#pretest stats
w_stats <- summarize_column_stats(analysis_data, WordProcPretest)
s_stats <- summarize_column_stats(analysis_data, SpreadsheetsPretest)
p_stats <- summarize_column_stats(analysis_data, PresentationPretest)
d_stats <- summarize_column_stats(analysis_data, DatabasesPretest)
  

data <- data.frame(
  Evaluation_item = rep(c("Word Processing", "Spreadsheets", "Presentations", "Databases"), each = 5),
  Group = rep(c("Control", "Ed. Game", "Gamification", "Social", "Social Gamif."), 4),
  N = rep(value_counts, 4),
  Mean = c(w_stats$mean, s_stats$mean, p_stats$mean, d_stats$mean),
  Std_err = c(w_stats$se, s_stats$se, p_stats$se, d_stats$se),
  Std_dev = c(w_stats$sd, s_stats$sd, p_stats$sd, d_stats$sd)
)

# Table 3

#posttest stats
w2_stats <- summarize_column_stats(analysis_data, WordProcPosttest)
s2_stats <- summarize_column_stats(analysis_data, SpreadsheetsPosttest)
p2_stats <- summarize_column_stats(analysis_data, PresentationPosttest)
d2_stats <- summarize_column_stats(analysis_data, DatabasesPosttest)


data2 <- data.frame(
  Evaluation_item = rep(c("Word Processing", "Spreadsheets", "Presentations", "Databases"), each = 5),
  Group = rep(c("Control", "Ed. Game", "Gamification", "Social", "Social Gamif."), 4),
  N = rep(value_counts, 4),
  Mean = c(w2_stats$mean, s2_stats$mean, p2_stats$mean, d2_stats$mean),
  Std_err = c(w2_stats$se, s2_stats$se, p2_stats$se, d2_stats$se),
  Std_dev = c(w2_stats$sd, s2_stats$sd, p2_stats$sd, d2_stats$sd)
)

### Figures ###

#Figure 5

#Figure 6



