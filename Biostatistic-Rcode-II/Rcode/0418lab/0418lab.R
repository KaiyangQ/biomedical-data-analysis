# Set working path
getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/0418lab")

# Load the data
#data <- read.csv("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/0418lab/scalars_trim.csv")

# Install Tidyverse (includes dplyr)
install.packages("tidyverse")

# Load Tidyverse (or just dplyr)
library(tidyverse)  # This will load dplyr and other useful packages
# or
library(dplyr)

#store directory where your data is kept and load data
pupil = read.csv("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/0418lab/scalars_trim.csv")
## New names:
## Rows: 373 Columns: 7
## -- Column specification
## -------------------------------------------------------- Delimiter: "," chr
## (4): subject_id, user_type, prepost, eye dbl (3): ...1, min_constriction, AUC
## i Use `spec()` to retrieve the full column specification for this data. i
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## * `` -> `...1`
pupil = pupil %>%
  # factor and arrange data
  mutate(user_type = factor(user_type, levels = c("non-user", "occasional", "daily")),
         prepost = factor(prepost, levels = c("pre2", "post")),
         eye = factor(eye, levels = c("Right", "Left")),
         auc_bin = ifelse(AUC > -20, "small", "large"),
         percent_constriction = min_constriction * -1) %>%
  select(subject_id, user_type, prepost, eye, percent_constriction, AUC, auc_bin) %>%
  # make sure data is sorted properly
  arrange(subject_id, prepost, eye)


# Load necessary libraries
library(dplyr)

# Load your data
pupil <- read.csv("path_to_your_csv/scalars_trim.csv")

# 1. Count the number of unique subjects
number_of_subjects <- pupil %>%
  distinct(subject_id) %>%
  nrow()

# 2. Determine how many timepoints each subject has
timepoints_per_subject <- pupil %>%
  group_by(subject_id) %>%
  summarise(count = n_distinct(prepost))

# 3. Check for any missing values
missing_values <- sum(is.na(pupil))

# Output the results
print(paste("Total number of unique subjects:", number_of_subjects))
print("Timepoints per subject:")
print(timepoints_per_subject)
print(paste("Total missing values in the dataset:", missing_values))
