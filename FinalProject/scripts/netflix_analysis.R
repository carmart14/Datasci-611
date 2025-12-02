# =============================
# Data Science Project 
# 
# 
# 
# ============================

install.packages("factoextra")
install.packages("pheatmap")


library(pheatmap)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(factoextra)
library(purrr)
library(lme4)
library(reshape2)


users_data <- readRDS(file = "data/processed/user_data.rds")
mov_show_data <- readRDS(file = "data/processed/mov_show_data.rds")



#
mov_show_data <- mov_show_data |> 
  mutate(
    duration_num = as.numeric(str_extract(Duration, "\\d+")),
    duration_unit = str_extract(Duration, "min|Season|Seasons")
  )


#  overall content count for each country
mov_show_data |> 
  mutate(Country = as.factor(Country)) |> 
  group_by(Country) |> 
  reframe(count = n())

# number of shows and movies per country
content_count <- mov_show_data |> 
  group_by(Country, Type) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

# visualization
ggplot(content_count, aes(x = Country, y = count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Movie" = "red", "TV Show" = "blue")) +
  labs(title = "Count of Movies and TV Shows by Country",
       x = "Country",
       y = "Count",
       fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# save object for eaiser plotting in the makefile 
saveRDS(content_count, file = "data/processed/content_count.rds")

# average duration for each show and movie per country
mov_show_data <- mov_show_data |> 
  mutate(
    # Extract numeric value and unit
    duration_num = as.numeric(str_extract(Duration, "\\d+")),
    duration_unit = str_extract(Duration, "min|Season|Seasons")
  ) |> 
  mutate(
    normalized_duration = case_when(
      Type == "Movie" & duration_unit == "min" ~ duration_num,
      
      # Movies (1 season = 90 min)
      Type == "Movie" & duration_unit %in% c("Season", "Seasons") ~ 90 * duration_num,
      
      Type == "TV Show" & duration_unit == "min" ~ duration_num,
      
      # TV Shows in seasons ( 10 eps/season × 45 min)
      Type == "TV Show" & duration_unit %in% c("Season", "Seasons") ~ duration_num * 10 * 45,
      
      # Catch all else
      TRUE ~ NA_real_
    ),
    # Create duration per episode for TV Shows
    duration_per_episode = case_when(
      Type == "TV Show" & duration_unit %in% c("Season", "Seasons") ~ 45, # assuming 45 mins per episode 
      Type == "TV Show" & duration_unit == "min" ~ duration_num,          
      TRUE ~ NA_real_
  )
)


# Summarize: average normalized duration per country / genre / type
avg_duration_summary <- mov_show_data %>%
  group_by(Country, Genre, Type) %>%
  summarise(
    avg_duration = mean(normalized_duration, na.rm = TRUE),
    avg_duration_per_episode = mean(duration_per_episode, na.rm = TRUE),
    n_titles = n(),
    .groups = "drop"
  )

head(avg_duration_summary)

# saving object for eaiser plotting in the makefile
saveRDS(avg_duration_summary, file = "data/processed/avg_duration_summary.rds")



# dim reduction

genre_mat <- mov_show_data |> 
  count(Country, Genre) |>           # count titles per genre per country
  group_by(Country) |> 
  mutate(prop = n / sum(n)) |>       # convert to proportions
  ungroup() |> 
  select(-n) |> 
  pivot_wider(names_from = Genre, values_from = prop, values_fill = 0)

saveRDS(genre_mat, file = "data/processed/genre_mat.RDS")










# ranking the shows =====



# Define numeric mapping
rating_map <- c(
  "G" = 1, "TV-G" = 1,
  "PG" = 2, "TV-PG" = 2,
  "PG-13" = 3, "TV-14" = 3,
  "R" = 4, "TV-MA" = 5
)

data_numeric <- mov_show_data |> 
  mutate(Rating_num = rating_map[Rating])

avg_rating <- data_numeric |> 
  group_by(Country, Type) |> 
  summarise(
    avg_rating = mean(Rating_num, na.rm = TRUE), # account for multiple titles
    .groups = "drop"
  )



avg_rating_wide <- avg_rating %>%
  pivot_wider(names_from = Type, values_from = avg_rating) %>%
  mutate(diff = Movie - `TV Show`) # positive = movies more restricted



overall_rating <- data_numeric %>%
  group_by(Country) %>%
  summarise(
    overall_avg = mean(Rating_num),
    .groups = "drop"
  ) %>%
  arrange(overall_avg) # most general → most restricted





country_type_summary <- data_numeric %>%
  group_by(Country, Type) %>%
  summarise(
    avg_rating = mean(Rating_num, na.rm = TRUE),
    n_titles = n(),
    .groups = "drop"
  )

saveRDS(country_type_summary, file = "data/processed/country_type_summary.RDS")

# comparing the counties indivvual and how the ratings compare

country_genre_summary <- data_numeric %>%
  group_by(Country, Genre) %>%
  summarise(
    avg_rating = mean(Rating_num, na.rm = TRUE),
    n_titles = n(),
    .groups = "drop"
  )


saveRDS(country_genre_summary, file = "data/processed/country_genre_summary.RDS")



















# results over all countries
results_type <- mov_show_data |> 
  group_by(Country) |> 
  count(Type) |> 
  pivot_wider(names_from = Type, values_from = n, values_fill = 0) %>%
  mutate(
    p_value = map2_dbl(`Movie`, `TV Show`, ~ {
      tbl <- matrix(c(.x, .y), nrow = 2)
      chisq.test(tbl)$p.value
    })
  )

# split dataset by country
country_list <- split(mov_show_data, mov_show_data$Country)

genre_tests <- map(country_list, function(df) {
  tab <- table(df$Genre, df$Type)
  
  # some small tables need Fisher’s exact test
  if(any(tab < 5)) {
    test <- fisher.test(tab)
  } else {
    test <- chisq.test(tab)
  }
  
  list(
    test = test,
    p_value = test$p.value,
    table = tab
  )
})

prop_plot <- mov_show_data %>% 
  group_by(Country, Genre, Type) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  group_by(Country, Type) %>% 
  mutate(prop = n / sum(n))

ann_data <- results_type %>% 
  select(Country, p_value) %>% 
  mutate(label = paste0("p = ", signif(p_value, 2)))

ggplot(prop_plot, aes(x = Country, y = prop, fill = Genre)) +
  geom_col(position = "fill") +
  facet_wrap(~Type) +
  geom_text(
    data = ann_data,
    aes(x = Country, y = 1.05, label = label),
    inherit.aes = FALSE
  ) +
  coord_cartesian(ylim = c(0, 1.1), clip = "off") 







