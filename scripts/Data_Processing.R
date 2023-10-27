library(tidyverse)
library(dplyr)
library(readr)

root_dir <- rprojroot::find_rstudio_root_file()
data_dir <- file.path(root_dir, "data")
setwd(data_dir)

# Import the two CSV files
interest_clean <- read.csv("interest-clean.csv")
background_clean <- read.csv("background-clean.csv")

# Check the first few rows of each dataset
head(interest_clean)
head(background_clean)


merged_data <- merge(interest_clean, background_clean, by = "response.id")
head(merged_data)

### One hot coding part ###
# One-hot encoding function using tidyverse
one_hot_encode <- function(data, column_name) {
  data %>%
    separate_rows({{ column_name }}, sep = ";") %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = {{ column_name }}, values_from = value, names_prefix = paste0(column_name, "."), values_fill = 0) %>%
    group_by(response.id) %>%
    summarise(across(starts_with(paste0(column_name, ".")), max, .names = "{.col}"))
}

# One-hot encoding for both columns
dom_encoded <- one_hot_encode(merged_data, "dom.x")
area_encoded <- one_hot_encode(merged_data, "area")
lang_encoded <- one_hot_encode(merged_data, 'lang')
type_encoded <- one_hot_encode(merged_data, 'type')

joined_data <- lang_encoded %>%
  left_join(dom_encoded, by = "response.id") %>%
  left_join(area_encoded, by = "response.id") %>%
  left_join(type_encoded, by = "response.id")

# merge it with the original data
merged_data <- merge(merged_data, joined_data, by = "response.id")
merged_data <- merged_data %>% select(-dom.x, -area, -lang, -type)

### Change the type of metrics
merged_data <- merged_data %>%
  mutate(across(where(is.character), 
                ~as.numeric(case_when(
                  . %in% c("yes", "Yes", "true", "True", "TRUE") ~ "1",
                  . %in% c("no", "No", "false", "False", "FALSE") ~ "0",
                  . %in% c("unsure", "Unsure") ~ "-1",
                  . %in% c("Adv") ~ "9",
                  . %in% c("Int") ~ "5",
                  . %in% c("Beg") ~ "3",
                  . %in% c("0-2") ~ "1",
                  . %in% c("5-Mar") ~ "2",
                  . %in% c("8-Jun") ~ "3",
                  . %in% c("9+") ~ "4",
                  TRUE ~ as.character(.)
                ))
  )) %>%
  # Convert all logical columns to 0 and 1
  mutate(across(where(is.logical), as.integer)) %>%
  # Adjust type.lab and type.ind based on type.both
  mutate(
    type.lab = ifelse(type.both == 1, 1, type.lab),
    type.ind = ifelse(type.both == 1, 1, type.ind)
  ) %>%
  # Adjust type.lab and type.ind based on type.both
  mutate(
    lang.Python = ifelse(`lang.I prefer to work in Python but I am still comfortable with R!` == 1, 1, lang.Python)
  )

write_csv(merged_data, "processed_data.csv")

