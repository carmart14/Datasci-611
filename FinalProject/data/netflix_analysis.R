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



data_user <- read.csv(file = "/Users/carmart/Downloads/DS611/FinalProject/Netflix_UserInfo/netflix_users.csv")


data_movsho |> 
  mutate(Country = as.factor(Country)) |> 
  group_by(Country) |> 
  reframe(count = n())

content_sumary <- data_movsho |> 
  group_by(Type, Country, Genre) |> 
  summarise(count = n()) |> 
  arrange(desc(count))


content_count <- data_movsho |> 
  group_by(Country, Type) |> 
  summarise(count = n()) |> 
  arrange(desc(count))


ggplot(content_count, aes(x = Country, y = count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Movie" = "red", "TV Show" = "blue")) +
  labs(title = "Count of Movies and TV Shows by Country",
       x = "Country",
       y = "Count",
       fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(content_sumary, aes(x = Type, y = count, fill = Type)) +
  geom_col(position = "dodge") +
  facet_grid(Country ~ Genre) +
  labs(
    title = "Genre Popularity by Country (Movies vs TV Shows)",
    x = "Type",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  guides(fill = "none")

# same as above but simpler so put into props not raw values

genre_props <- content_sumary %>%
  group_by(Country, Type) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

ggplot(genre_props, aes(x = Genre, y = prop, fill = Type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Movie" = "red", "TV Show" = "blue")) +
  facet_wrap(~ Country, scales = "free_x", ncol = 4) +
  labs(title = "Genre Preference Proportions by Country",
       y = "Proportion")


# preference scoring 

pref_score <- content_sumary |> 
  pivot_wider(names_from = Type, values_from = count, values_fill = 0) |> 
  mutate(
    preference = (Movie - `TV Show`) / (Movie + `TV Show`) # keep the backticks to account for the space char
  )



ggplot(genre_props, aes(x = prop, y = Genre, color = Country)) +
  geom_point(aes(size = count), alpha = 0.8) +
  labs(
    title = "Genre Proportions by Country",
    x = "Proportion",
    y = "Genre",
    size = "Count"
  ) +
  theme_minimal()



ggplot(genre_props, aes(x = Genre, y = prop, fill = Country)) +
  geom_col(position = "fill") +  
  facet_wrap(~ Type, ncol = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Genre Proportions by Country",
    x = "Genre",
    y = "Proportion (%)",
    fill = "Country"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )



# dim reduction



genre_mat <- data_movsho %>%
  count(Country, Genre) %>%          # count titles per genre per country
  group_by(Country) %>%
  mutate(prop = n / sum(n)) %>%      # convert to proportions
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = Genre, values_from = prop, values_fill = 0)


genre_pca <- prcomp(genre_mat %>% select(-Country), scale. = TRUE)



fviz_pca_biplot(
  genre_pca,
  geom.ind = "point",
  habillage = genre_mat$Country,
  repel = TRUE
)


library(ggplot2)
library(reshape2)

genre_cor <- cor(genre_mat |> select(-Country))

# correlation heatmap
ggplot(melt(genre_cor), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2() +
  theme_minimal()





mat <- genre_mat |> select(-Country)
rownames(mat) <- genre_mat$Country

pheatmap(
  mat,
  scale = "none",        # or "row" to normalize within country
  clustering_method = "complete",
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  main = "Genre Proportion Heatmap by Country"
)


# country similarity 
country_dist <- dist(genre_mat |> select(-Country))
heatmap(as.matrix(country_dist))


# heirical clustering
hc <- hclust(dist(genre_mat |> select(-Country)))
plot(hc)
plot(hc, labels = genre_mat$Country, main = "Country Clustering by Genre Preferences")




top_genre_per_country <- content_sumary |> 
  group_by(Country, Type) |> 
  slice_max(count, n = 2)



# ranking the shows =====



# Define numeric mapping
rating_map <- c(
  "G" = 1, "TV-G" = 1,
  "PG" = 2, "TV-PG" = 2,
  "PG-13" = 3, "TV-14" = 3,
  "R" = 4, "TV-MA" = 5
)

data_numeric <- data_movsho %>%
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





ggplot(avg_rating, aes(x = Country, y = avg_rating, fill = Type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(breaks = 1:4, labels = c("G/TV-G","PG/TV-PG","PG-13/TV-14","R/TV-MA")) +
  labs(y = "Average Rating (More Restricted → Higher)", title = "Average Rating by Country and Type") +
  theme_minimal()


country_type_summary <- data_numeric %>%
  group_by(Country, Type) %>%
  summarise(
    avg_rating = mean(Rating_num, na.rm = TRUE),
    n_titles = n(),
    .groups = "drop"
  )


ggplot(country_type_summary,
       aes(x = avg_rating, y = reorder(Country, avg_rating), color = Type)) +
  geom_point(size = 4) +
  labs(
    title = "Average Rating of Movies vs TV Shows by Country",
    x = "Average Rating",
    y = "Country"
  ) +
  theme_minimal(base_size = 14)

# comparing the counties indivvual and how the ratings compare

country_genre_summary <- data_numeric %>%
  group_by(Country, Genre) %>%
  summarise(
    avg_rating = mean(Rating_num, na.rm = TRUE),
    n_titles = n(),
    .groups = "drop"
  )



ggplot(country_genre_summary,
       aes(x = Genre, y = avg_rating, color = Genre)) +
  geom_point(size = 3, alpha = 0.9) +
  facet_wrap(~ Country, ncol = 4) +
  labs(
    title = "Genre Restriction Levels by Country (Ranked)",
    x = "Genre",
    y = "Average Rating (Higher = More Restricted)"
  ) +
  theme_light(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )




# stat stuff =======



# results over all countries
results_type <- data_movsho %>%
  group_by(Country) %>%
  count(Type) %>%
  pivot_wider(names_from = Type, values_from = n, values_fill = 0) %>%
  mutate(
    p_value = map2_dbl(`Movie`, `TV Show`, ~ {
      tbl <- matrix(c(.x, .y), nrow = 2)
      chisq.test(tbl)$p.value
    })
  )

# split dataset by country
country_list <- split(data_movsho, data_movsho$Country)

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





## sig testing to see if countries perfer a specific rating compared to others



model <- lmer(
  Rating_num ~ Type + (1 | Country),
  data = data,
  weights = count
)

summary(model)




