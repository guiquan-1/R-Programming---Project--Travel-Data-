#hypothesis 4: Age group affect the destination of the trip

# Variables required: Age group and Travel Destination
#Income: AGRDE_FLAG_NM
#Destination: TOUR_CTPRVN_NM

#code to clean the data
#since a lot of people who visited metropolitan cities answered "모름" on TOUR_SIGNGU_NM I skipped cleaning those data 

# Chi-square test to see if there's a relationship between age group and destination
table_age_dest <- table(df_tour_era2$AGRDE_FLAG_NM, df_tour_era2$TOUR_CTPRVN_NM)
chisq_test_dest <- chisq.test(table_age_dest)
print(chisq_test_dest) #Result: X-squared = 317.75, df = 60, p-value < 2.2e-16

unique(df_tour_era2$TOUR_CTPRVN_NM)

ggplot(df_tour_era2, aes(x = AGRDE_FLAG_NM, fill = TOUR_CTPRVN_NM)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Destinations by Age Group (Proportional)",
    x = "Age Group",
    y = "Proportion",
    fill = "Destinations"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#What if we categorize destinations into metropolitan cities and general cities?
df_tour_era2 <- df_tour_era2 %>%
  mutate(destination_type = case_when(
    TOUR_CTPRVN_NM %in% c("서울시", "부산시", "대구시", "인천시", "광주시", "대전시", "울산시", "제주시") ~ "Metropolitan City",
    !is.na(TOUR_CTPRVN_NM) ~ "General City",
    TRUE ~ NA_character_
  )) %>%
  relocate(destination_type, .after = TOUR_CTPRVN_NM)

#Visualize
ggplot(df_tour_era2, aes(x = AGRDE_FLAG_NM, fill = destination_type)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Destination Type Proportion by Age Group",
    x = "Age Group",
    y = "Proportion",
    fill = "Destination Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Chi-square test to see if there's a relationship between age group and destination type
table_age_dest_type <- table(df_tour_era2$AGRDE_FLAG_NM, df_tour_era2$destination_type)
chisq_test_dest_type <- chisq.test(table_age_dest_type)
print(chisq_test_dest_type) #Result: X-squared = 57.058, df = 4, p-value = 1.203e-11

#It seems as people get older, they visit genral cities more than metropolitan cities? BUT it's not true for 40s

chisq_result <- chisq.test(table(df_tour_era2$AGRDE_FLAG_NM, df_tour_era2$destination_type))
chisq_result$observed
chisq_result$expected

