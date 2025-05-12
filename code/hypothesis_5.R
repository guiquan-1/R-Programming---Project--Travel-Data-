#use df_tour_era

glimpse(df_tour_era)

### hypothesis 5: Income affects the purpose of the trip

# Variables required: Income and Travel
#Income: HSHLD_INCOME_DGREE_NM
#Travel Purpose: TOUR_PURPS_NM

# We see that income had 모름 entries when checking for missing entries earlier

# remove all rows with 모름 for HSHLD_INCOME_DGREE_NM
df_hypo_5 <- df_tour_era %>%
  filter(HSHLD_INCOME_DGREE_NM != "모름")

glimpse(df_hypo_5) #okay checked that it was removed - done!

#because of funny entries
valid_income_levels <- c("300만원 미만", "300~500만원 미만", "500~700만원 미만", "700만원 이상")
df_hypo_5 <- df_hypo_5 %>%
  filter(HSHLD_INCOME_DGREE_NM %in% valid_income_levels)

#converting to factors
df_hypo_5 <- df_hypo_5 %>%
  mutate(
    HSHLD_INCOME_DGREE_NM = factor(HSHLD_INCOME_DGREE_NM, 
                                   levels = c("300만원 미만", "300~500만원 미만", 
                                              "500~700만원 미만", "700만원 이상"),
                                   ordered = TRUE),
    SEXDSTN_FLAG_CD = factor(SEXDSTN_FLAG_CD),        # Gender
    AGRDE_FLAG_NM   = factor(AGRDE_FLAG_NM, 
                             levels = c("20대", "30대", "40대", "50대", "60대 이상"), 
                             ordered = TRUE),
    MRRG_AT_NM      = factor(MRRG_AT_NM),             # Marital status
    CHLDRN_TY_NM    = factor(CHLDRN_TY_NM),           # Child type
    OCCP_NM         = factor(OCCP_NM),                # Occupation
    TOUR_PURPS_NM   = factor(TOUR_PURPS_NM,
                             levels = c(
                               "식도락 (지역 특색/제철 음식)",
                               "놀이/테마공원-온천 등 즐기기",
                               "자연 풍경 감상 (산/ 바다 등)",
                               "휴식",
                               "문화-예술 즐기기 (미술관/ 공연 등)",
                               "역사-유적 감상 (유적지/ 박물관 등)",
                               "취미-운동 활동 (등산/ 낚시/ 골프 등)",
                               "친지/친구/친척 만나기",
                               "도시 경관 감상 (건축물/ 거리 등)",
                               "쇼핑",
                               "축제-행사 참여",
                               "기타"
                             )
    ),
    TOUR_PD_VALUE   = factor(TOUR_PD_VALUE),          # Travel duration
    TOUR_COM_NMPR_NM = factor(TOUR_COM_NMPR_NM),      # Travel group size
    COM_ONE_TY      = factor(COM_ONE_TY),             # Companion types
    COM_TWO_TY      = factor(COM_TWO_TY),
    COM_THREE_TY    = factor(COM_THREE_TY)
  )



# visualise household income
ggplot(df_hypo_5, aes(x = HSHLD_INCOME_DGREE_NM)) +
  geom_bar(fill = "skyblue", alpha = 0.8) +
  labs(title = "Household Income Distribution",
       x = "Household Income Level", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )


#visualise tour purpose
ggplot(df_hypo_5, aes(x = TOUR_PURPS_NM)) +
  geom_bar(fill = "tomato", alpha = 0.8) +
  labs(title = "Travel Purpose",
       x = "Travel Purpose", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

#compare income to travel purpose
ggplot(df_hypo_5, aes(x = HSHLD_INCOME_DGREE_NM, fill = TOUR_PURPS_NM)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Travel Purpose by Household Income",
       x = "Household Income Level", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#to confirm relationship
chisq.test(table(df_hypo_5$HSHLD_INCOME_DGREE_NM, df_hypo_5$TOUR_PURPS_NM))




