### hypothesis 6: Income affects the destination of the trip

# Variables required: Income and Travel
#Income: HSHLD_INCOME_DGREE_NM
#Destination: TOUR_CTPRVN_NM

# We see that income had 모름 entries when checking for missing entries earlier

# remove all rows with 모름 for HSHLD_INCOME_DGREE_NM
df_hypo_6 <- df_tour_era %>%
  filter(HSHLD_INCOME_DGREE_NM != "모름")



# Filter out '모름' income
df_plot <- df_tour_era %>%
  filter(HSHLD_INCOME_DGREE_NM != "모름")

# Plot
ggplot(df_plot, aes(x = fct_infreq(TOUR_CTPRVN_NM), fill = TOUR_CTPRVN_NM)) +
  geom_bar(show.legend = FALSE) +
  facet_wrap(~ HSHLD_INCOME_DGREE_NM, scales = "free_x") +
  coord_flip() +
  labs(
    title = "Travel Destination Distribution by Household Income Level",
    x = "Province/Destination",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(strip.text = element_text(face = "bold", size = 12))



# Perform Chi-squared test for income and destination
chisq_test_income_destination <- chisq.test(
  table(df_plot$HSHLD_INCOME_DGREE_NM, df_plot$TOUR_CTPRVN_NM)
)

# Print the result
print(chisq_test_income_destination)



#because of funny entries
valid_income_levels <- c("300만원 미만", "300~500만원 미만", "500~700만원 미만", "700만원 이상")
df_hypo_6 <- df_hypo_5 %>%
  filter(HSHLD_INCOME_DGREE_NM %in% valid_income_levels)


#converting to factors
df_hypo_6 <- df_hypo_6 %>%
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
    COM_THREE_TY    = factor(COM_THREE_TY),
    
    TOUR_CTPRVN_NM = factor(TOUR_CTPRVN_NM,
                            levels = c(
                              "강원도", "경상남도", "전라북도", "부산시", "제주도", "서울시", "경기도",
                              "대구시", "충청남도", "충청북도", "전라남도", "경상북도", "광주시", 
                              "대전시", "인천시", "울산시"
                            ))
  )

ggplot(df_hypo_6, aes(x = HSHLD_INCOME_DGREE_NM, fill = TOUR_CTPRVN_NM)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Destination by Household Income Level",
       x = "Income Level", y = "Proportion of Respondents",
       fill = "Province Visited") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



# to check - dont have to submit
# Contingency table
table_income_dest <- table(df_hypo_6$HSHLD_INCOME_DGREE_NM, df_hypo_6$TOUR_CTPRVN_NM)
# Chi-squared test
chisq.test(table_income_dest)

