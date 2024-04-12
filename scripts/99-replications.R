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
library(gridExtra)

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
plot1 <- ggplot(analysis_data, aes(x = Group, y = WordProcPosttest, color = Group)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter(alpha = 0.3, width = 0.15, height = 0) +
  labs(x = "Group Type", y = "Word Processing") +
  theme(axis.text.x = element_blank())

plot2 <- ggplot(analysis_data, aes(x = Group, y = SpreadsheetsPosttest, color = Group)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter(alpha = 0.3, width = 0.15, height = 0) +
  labs(x = "Group Type", y = "Presentations") +
  theme(axis.text.x = element_blank())

plot3 <- ggplot(analysis_data, aes(x = Group, y = PresentationPosttest, color = Group)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter(alpha = 0.3, width = 0.15, height = 0) +
  labs(x = "Group Type", y = "Word Processing") +
  theme(axis.text.x = element_blank())

plot4 <- ggplot(analysis_data, aes(x = Group, y = DatabasesPosttest, color = Group)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter(alpha = 0.3, width = 0.15, height = 0) +
  labs(x = "Group Type", y = "Databases") +
  theme(axis.text.x = element_blank())

fig5 <- grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)
ggsave("other/figures/figure5.png", width = 10, height = 8, fig5) 

#Figure 6
fig6 <- ggplot(analysis_data, aes(x = Group, y = FinalExamination, color = Group)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter(alpha = 0.3, width = 0.15, height = 0) +
  labs(x = "Group Type", y = "Final Examination Grade") +
  theme(axis.text.x = element_blank())

ggsave("other/figures/figure6.png", fig6)
