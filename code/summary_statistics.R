file_list <- list.files(path = "data", pattern = "*.csv", full.names = TRUE)
df_tour_era <- file_list %>%
  lapply(read_csv) %>%      # read each file into a list
  bind_rows()               # combine them into one big dataframe

#dataset glimpse # number of entries and number of variables
glimpse(df_tour_era)

# View the first few rows
head(df_tour_era)

# View the structure (columns, types)
str(df_tour_era)

# Summary statistics (numeric + categorical)
summary(df_tour_era)

# Dimensions (rows and columns)
dim(df_tour_era)

# Column names
names(df_tour_era)

#More Descriptive Stats
# Load needed libraries
library(dplyr)

# Frequency and proportion of gender
df_tour_era %>%
  count(SEXDSTN_FLAG_CD) %>%
  mutate(Percent = n / sum(n) * 100)

# Prepare top 10 travel purposes
top_purposes <- df_tour_era %>%
  count(TOUR_PURPS_NM, sort = TRUE) %>%
  slice_max(n, n = 10)

# Plot with x-axis as travel purpose
ggplot(top_purposes, aes(x = reorder(TOUR_PURPS_NM, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(
    title = "Top 10 Travel Purposes",
    x = "Travel Purpose",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




