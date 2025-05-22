# Load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(forcats)

file_list <- list.files(path = "data", pattern = "*.csv", full.names = TRUE)
df_tour_era <- file_list %>%
  lapply(read_csv) %>%      # read each file into a list
  bind_rows()               # combine them into one big dataframe
