
# Import the files
df_tour_era   <- read.csv("data/CI_TOUR_ERA_CHARTR_INFO_20250331.csv", stringsAsFactors = FALSE)
print(df_tour_era)

#Basic Data Visualization
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

# Top 10 most common travel purposes
df_tour_era %>%
  count(TOUR_PURPS_NM, sort = TRUE) %>%
  top_n(10)




