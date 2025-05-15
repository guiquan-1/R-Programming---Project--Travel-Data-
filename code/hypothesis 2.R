# Load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)



### Data Preparation
file_list <- list.files(path = "data", pattern = "*.csv", full.names = TRUE)
df_tour_era <- file_list %>%
  lapply(read_csv) %>%      # read each file into a list
  bind_rows()               # combine them into one big dataframe

# Check result
glimpse(df_tour_era)

# Variables required: 1) type of travel companion; 2) destination
# Type of travel companion: COM_ONE_TY 
# Destination: TOUR_CTPRVN_NM

# checking for missing data
colSums(is.na(df_tour_era)) #shows number
colSums(df_tour_era=='모름') #shows 모름

# code to clean the data - remove '모름' from destination list 
df_clean_destination <- df_tour_era %>%
  filter(TOUR_CTPRVN_NM != "모름")

# code to clean the data - remove '모름' from companion list 
df_clean_companion <- df_clean_destination %>%
  filter(COM_ONE_TY != "모름")

glimpse(df_clean_companion)

# code to clean the data - remove 'NA' from companion list 
df_clean_companion <- df_clean_companion %>%
  filter(!is.na(COM_ONE_TY), COM_ONE_TY != "")

# set 5 categories of 'companion'
df_clean_companion$COM_ONE_TY <- factor(df_clean_companion$COM_ONE_TY,
                                        levels = c("혼자서", "친구", "연인", "배우자", "부모/형제/자녀 등 가족")   
)


### option 1 - stacked bar graph categorized by companion (16개 특별시 및 자치도)
custom_order <- c(
  "서울시", "인천시", "경기도", "강원도", 
  "충청남도","충청북도", "대전시", "경상북도",
  "전라북도", "대구시", "경상남도", "울산시", 
  "전라남도", "광주시", "부산시", "제주도"
)

# Summarize counts
summary_data <- df_clean_companion %>%
  count(COM_ONE_TY, TOUR_CTPRVN_NM)

# Custom order
summary_data <- summary_data %>%
  mutate(TOUR_CTPRVN_NM = factor(TOUR_CTPRVN_NM, levels = custom_order))

# Get rid of NA
summary_data <- summary_data %>%
  filter(!is.na(COM_ONE_TY), !is.na(TOUR_CTPRVN_NM))

# Create stacked bar graph - categorized by companion
ggplot(summary_data, aes(x = COM_ONE_TY, y = n, fill = TOUR_CTPRVN_NM)) +
  geom_bar(stat = "identity") +
  labs(title = "Destination Breakdown by Travel Companion Type",
       x = "Travel Companion Type",
       y = "Count",
       fill = "Destination") +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









### option 2 - stacked bar graph categorized by destination 
# code to clean the data - remove 'NA' from companion list 
df_clean_companion <- df_clean_companion %>%
  filter(!is.na(COM_ONE_TY), COM_ONE_TY != "")

# set 5 categories of x-variables
df_clean_companion$COM_ONE_TY <- factor(df_clean_companion$COM_ONE_TY,
                                        levels = c("혼자서", "친구", "연인", "배우자", "부모/형제/자녀 등 가족")  
)

# Summarize counts
summary_data <- df_clean_companion %>%
  count(TOUR_CTPRVN_NM, COM_ONE_TY)

# Create stacked bar graph - categorized by destination
ggplot(summary_data, aes(x = TOUR_CTPRVN_NM, y = n, fill = COM_ONE_TY)) +
  geom_bar(stat = "identity") +
  labs(title = "Travel Companions by Destination",
       x = "Destination",
       y = "Count",
       fill = "Companion Type") +
  theme_minimal(base_family = "AppleGothic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


