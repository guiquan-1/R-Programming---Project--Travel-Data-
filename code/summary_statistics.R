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



#starting from variables
df_tour_era %>%
  filter(!is.na(TOUR_PURPS_NM)) %>%
  count(TOUR_PURPS_NM, sort = TRUE) %>%
  ggplot(aes(x = reorder(TOUR_PURPS_NM, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Frequency of Travel Purposes",
    x = "Purpose of Travel",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

df_tour_era %>%
  count(TOUR_CTPRVN_NM, sort = TRUE) %>%
  ggplot(aes(x = reorder(TOUR_CTPRVN_NM, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Frequency of Travel Destinations",
    x = "Province (Destination)",
    y = "Number of Respondents"
  ) +
  theme_minimal()

df_tour_era %>%
  filter(HSHLD_INCOME_DGREE_NM != "모름") %>%
  count(HSHLD_INCOME_DGREE_NM) %>%
  ggplot(aes(x = reorder(HSHLD_INCOME_DGREE_NM, n), y = n)) +
  geom_col(fill = "steelblue") +
  #coord_flip() +
  labs(
    title = "Distribution of Household Income Levels",
    x = "Household Income Level",
    y = "Number of Respondents"
  ) +
  theme_minimal()

df_tour_era %>%
  ggplot(aes(x = fct_infreq(AGRDE_FLAG_NM))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Respondents by Age Group",
    x = "Age Group",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


df_tour_era %>%
  filter(TOUR_COM_NMPR_NM != "모름") %>%  # Optional: exclude unclear answers
  ggplot(aes(x = forcats::fct_infreq(TOUR_COM_NMPR_NM))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Respondents by Travel Group Size",
    x = "Travel Group Size",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

valid_groups <- c("혼자서", "2명", "3명", "4명", "5명 이상")

df_tour_era %>%
  filter(TOUR_COM_NMPR_NM %in% valid_groups) %>%
  mutate(TOUR_COM_NMPR_NM = factor(TOUR_COM_NMPR_NM,
                                   levels = valid_groups)) %>%
  ggplot(aes(x = TOUR_COM_NMPR_NM)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Respondents by Travel Group Size",
       x = "Travel Group Size",
       y = "Number of Respondents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


df_tour_era %>%
  select(COM_ONE_TY, COM_TWO_TY, COM_THREE_TY, COM_FOUR_TY, COM_FIVE_TY) %>%
  pivot_longer(cols = everything(), names_to = "CompanionSlot", values_to = "CompanionType") %>%
  filter(!is.na(CompanionType)) %>%
  count(CompanionType, sort = TRUE) %>%
  ggplot(aes(x = reorder(CompanionType, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Frequency of Travel Companion Types (Combined)",
       x = "Companion Type",
       y = "Number of Mentions") +
  theme_minimal()
