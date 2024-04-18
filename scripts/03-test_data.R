#### Preamble ####
# Purpose: Run tests on the analysis data
# Author: Sachin Chhikara
# Date: 8 April 2024
# Contact: sachin.chhikarar@utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(arrow)

analysis_data <- read_parquet("data/analysis_data/analysis_data.parquet")

## Helper functions
is_column_of_type <- function(dataframe, column_name, datatype) {
  column_class <- class(dataframe[[column_name]])
  return(datatype %in% column_class)
}

check_numeric_columns <- function(dataframe, column_names) {
  for (col in column_names) {
    if (is_column_of_type(dataframe, col, "numeric")) {
      message(paste(col, ": All values are numeric"))
    } else {
      message(paste(col, "Error: Not all values are numeric"))
    }
  }
}

check_gender_values <- function(dataframe, column_name, expected_values) {
  column_values <- dataframe[[column_name]]
  if (all(column_values %in% expected_values)) {
    message(paste(column_name, ": All values are valid"))
  } else {
    message(paste(column_name, "Error: Some values are not valid"))
  }
}

check_values_between_0_and_100 <- function(column_values_list) {
  for (i in seq_along(column_values_list)) {
    column_values <- column_values_list[[i]][[1]]
    column_name <- column_values_list[[i]][[2]]
    if (all(column_values >= 0 & column_values <= 100)) {
      message(paste(column_name, ":
                    The values are between 0 and 100 as expected"))
    } else {
      message(paste(
        column_name,
        "Error: There is an inappropriate value in this dataset.
                    Double check that the dataset was modified incorrectly or
                    remove that value from the dataset if necessary"
      ))
    }
  }
}

#### Test data ####
# 1.Proper datatype is used for each column
if (is_column_of_type(analysis_data, "Group", "character")) {
  "The numeric values in this column had their numeric values
  converted to their character represntation"
} else {
  "The numeric values have not been converted, please makes sure to
  run the 02_data_cleaning.R"
}


check_gender_values(analysis_data, "gender", c("male", "female"))

check_numeric_columns(analysis_data, c(
  "WordProcPretest",
  "SpreadsheetsPretest",
  "PresentationPretest",
  "DatabasesPretest"
))

check_numeric_columns(analysis_data, c(
  "WordProcPosttest",
  "SpreadsheetsPosttest",
  "PresentationPosttest",
  "DatabasesPosttest"
))
check_numeric_columns(analysis_data, "FinalExamination")


# 2. Boundary Conditions
check_values_between_0_and_100(list(
  list(analysis_data$WordProcPretest, "WordProcPretest"),
  list(analysis_data$SpreadsheetsPretest, "SpreadsheetsPretest"),
  list(analysis_data$PresentationPretest, "PresentationPretest"),
  list(analysis_data$DatabasesPretest, "DatabasesPretest"),
  list(analysis_data$WordProcPosttest, "WordProcPosttest"),
  list(analysis_data$SpreadsheetsPosttest, "SpreadsheetsPosttest"),
  list(analysis_data$PresentationPosttest, "PresentationPosttest"),
  list(analysis_data$DatabasesPosttest, "DatabasesPosttest"),
  list(analysis_data$FinalExamination, "FinalExamination")
))

# 3. Checking the number of variables
if (length(analysis_data) == 11) {
  "Apporiate number of varaibles are in the dataset"
} else {
  "Error: There are more or less variables that expected,
  please examine the dataset "
}

if (!"Age" %in% names(analysis_data)) {
  "The Age variable is removed as expected"
} else {
  "Error: Please run the the 02_data_cleaning to remove Age variable"
}


# 4. Checking that there is no missing data
if (!all(complete.cases(analysis_data))) {
  print("There is missing data in the dataframe.")
} else {
  print("There is no missing data in the dataframe.")
}
