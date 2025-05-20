# Load libraries
library(dplyr)
library(readr)
library(ggplot2)

### Data Preparation
file_list <- list.files(path = "data", pattern = "*.csv", full.names = TRUE)
df_tour_era <- file_list %>%
  lapply(read_csv) %>%      # read each file into a list
  bind_rows()               # combine them into one big dataframe

length(df_tour_era)
# Check result
glimpse(df_tour_era)

#checking for missing data
colSums(is.na(df_tour_era)) #shows number
colSums(df_tour_era=='모름') #shows 모름

#code to clean the data - pls transfer to required files 
df_clean_destination <- df_tour_era %>%
  filter(TOUR_SIGNGU_NM != "모름")

df_clean_companion <- df_clean_destination %>%
  filter(TOUR_COM_NMPR_NM != "모름")

glimpse(df_clean_companion)


