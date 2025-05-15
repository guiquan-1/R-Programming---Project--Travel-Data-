library(tidyverse)

# Transform to long format
df_long <- df_tour_era %>%
  pivot_longer(
    cols = COM_ONE_TY,            # 오직 COM_ONE_TY만 변환
    names_to = "companion_slot",  # 고정적으로 "COM_ONE_TY"가 들어감
    values_to = "companion_type"  # 실제 동행자 유형 값들
  )


# Remove NA values (Keep only rows where companion_type is not NA)
df_long_clean <- df_long %>%
  filter(!is.na(companion_type))


# Frequency table between Companion Type and Tour Purpose
table_companion_purpose <- df_long_clean %>%
  count(companion_type, TOUR_PURPS_NM) %>%
  spread(key = TOUR_PURPS_NM, value = n, fill = 0)


# Visualize companion type
ggplot(df_long_clean, aes(x = companion_type)) + 
  geom_bar(fill = "Light Green", alpha = 0.8) + 
  labs(title = "Companion Type Distribution",
       x = "Companion Type", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )


# Visualize Tour Purpose
ggplot(df_long_clean, aes(x = TOUR_PURPS_NM)) +
  geom_bar(fill = "light blue", alpha = 0.8) +
  labs(title = "Tour Purpose Distribution",
       x = "Tour Purpose", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )


# Tour Purpose proportion by Companion Type
df_long_clean %>%
  group_by(companion_type, TOUR_PURPS_NM) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(companion_type) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = companion_type, y = pct, fill = TOUR_PURPS_NM)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Percentage", x = "Companion type", fill = "Tour Purpose") +
  theme_minimal()
