# =============================================
# Data Science Project 
#
#
# ==============================================


users_data <- readRDS(file = "data/processed/clean_user_data.rds")
#
# analysis of how age is related to genre within countries
#

age_genre_summary <- users_data |> 
  group_by(Country, Favorite_Genre) |> 
  summarise(
    n = n(),
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE)
  )

write.csv(age_genre_summary, "data/processed/age_genre_summary.csv", row.names = FALSE)

# Anova per country
run_anova <- function(country_name) {
  df_sub <- users_data |>  filter(Country == country_name)
  model <- aov(Age ~ Favorite_Genre, data = df_sub)
  
  tidy_res <- broom::tidy(model)  |>  mutate(Country = country_name)
  
  list(model = model, tidy_res = tidy_res)
}

countries <- unique(users_data$Country)

age_genre_results <- map(countries, run_anova)


age_genre_anova <- map_dfr(age_genre_results, "tidy_res")

write.csv(age_genre_anova, "data/processed/age_genre_anova.csv", row.names = FALSE)

# Save all models (list) 
saveRDS(age_genre_results, "data/processed/age_genre_models.rds")

#
# Watch time analysis 
#

watch_summary <- users_data |> 
  group_by(Country, Subscription_Type) |> 
  summarise(
    n = n(),
    avg_watch = mean(Watch_Time_Hours, na.rm = TRUE),
    median_watch = median(Watch_Time_Hours, na.rm = TRUE),
    sd_watch = sd(Watch_Time_Hours, na.rm = TRUE)
  )


write.csv(watch_summary, "data/processed/watch_summary.csv", row.names = FALSE)


#
#  Subscription Anovas
#

subscription_model <- aov(
  Watch_Time_Hours ~ Subscription_Type * Country,
  data = users_data
)

subscription_anova_clean <- broom::tidy(subscription_model)

write.csv(subscription_anova_clean, "data/processed/subscription_two_way_anova.csv",
          row.names = FALSE)

# Save model for later figure generation
saveRDS(subscription_model, "data/processed/subscription_model.rds")




























































