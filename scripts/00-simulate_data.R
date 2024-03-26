#### Preamble ####
# Purpose: Simulates Postive effects of Gamification
# Author: Sachin Chhikara
# Date: 26 March 2024 
# Contact: sachin.chhikarar@utoronto.ca
# License: MIT


#### Workspace setup ####
# Load the tidyverse package
library(tidyverse)

# Set seed for reproducibility
set.seed(42)

# Number of samples
num_samples <- 1000

# Generate synthetic data
academic_performance <- rnorm(n = num_samples, mean = 70, sd = 15)
engagement <- rnorm(n = num_samples, mean = 60, sd = 20)
motivation <- rnorm(n = num_samples, mean = 75, sd = 10)
retention <- rnorm(n = num_samples, mean = 80, sd = 12)
satisfaction <- rnorm(n = num_samples, mean = 65, sd = 18)
gender <- sample(c("Male", "Female"), num_samples, replace = TRUE)

# Create tibble
data <- tibble(
  Gender = gender,
  Academic_Performance = academic_performance,
  Engagement = engagement,
  Motivation = motivation,
  Retention = retention,
  Satisfaction = satisfaction
)

# Ensure data is within the range of 0 to 100 (excluding Gender column)
data[, -1] <- mutate_all(data[, -1], ~ if_else(. < 0, 0, .))
data[, -1] <- mutate_all(data[, -1], ~ if_else(. > 100, 100, .))

# Round values to two decimal places (excluding Gender column)
data[, -1] <- round(data[, -1], 2)


data %>%
  gather(key = "Variable", value = "Value", -Gender) %>%
  ggplot(aes(x = Value, fill = Gender)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Comparison of Male and Female",
       x = "Value", y = "Frequency") +
  theme_minimal()





