#hypothesis 3: Age group affect the purpose of the trip

###Since there was some issue with encoding, I read the files again

file_list <- list.files(path = "data", pattern = "*.csv", full.names = TRUE)

# Function to guess encoding and read each file safely
read_with_correct_encoding <- function(file) {
  # Try to guess encoding
  enc <- guess_encoding(file, n_max = 1000)$encoding[1]  # Get most confident encoding
  message("📄 Reading ", file, " as ", enc)
  
  # Try to read with the guessed encoding
  tryCatch(
    read_csv(file, locale = locale(encoding = enc)),
    error = function(e) {
      message("❌ Failed: ", file)
      message("   Error: ", e$message)
      return(NULL)
    }
  )
}

# Read and combine all files
df_tour_era2 <- file_list %>%
  lapply(read_with_correct_encoding) %>%
  bind_rows()

unique(df_tour_era2$TOUR_PURPS_NM)

# Check result
glimpse(df_tour_era2)


# Variables required: Age group and Travel Purpose
#Income: AGRDE_FLAG_NM
#Travel Purpose: TOUR_PURPS_NM

# Chi-square test to see if there's a relationship between age group and purpose of the trip
table_age_purpose <- table(df_tour_era2$AGRDE_FLAG_NM, df_tour_era2$TOUR_PURPS_NM)
chisq_test_purpose <- chisq.test(table_age_purpose)
print(chisq_test_purpose) #Result: X-squared = 341.21, df = 44, p-value < 2.2e-16


# Since tour purposes are too varied, I filtered the top 5 purposes within each age group
top_purposes_per_age_group <- df_tour_era2 %>%
  group_by(AGRDE_FLAG_NM, TOUR_PURPS_NM) %>%
  count() %>%
  arrange(AGRDE_FLAG_NM, desc(n)) %>%
  group_by(AGRDE_FLAG_NM) %>%
  slice_max(n, n = 5) %>%
  ungroup()

glimpse(top_purposes_per_age_group)


#Calculate Proportions
top_purposes_per_age_group <- top_purposes_per_age_group %>%
  group_by(AGRDE_FLAG_NM) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() 

#Visualize
ggplot(top_purposes_per_age_group, aes(x = AGRDE_FLAG_NM, y = proportion, fill = TOUR_PURPS_NM)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Top 5 Trip Purposes by Age Group (Proportional)",
    x = "Age Group",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


