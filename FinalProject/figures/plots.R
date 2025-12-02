# =============================
# Datascience Final Project
# Plotting and Saving
# Author: Carmen Martin
# =============================



library(ggplot2)
library(scales)

##======= Movies and Shows Plots===============================================

# Load processed summary
genre_summary <- readRDS("data/processed/genre_summary.rds")
content_count <- readRDS("data/processed/content_count.rds")
genre_mat <- readDRS("data/processed/genre_mat.RDS")
country_genre_summary <- readRDS("data/processed/country_genre_summary.RDS")
country_type_summary <- readRDS("data/processed/country_type_summary.RDS")

# Create figures folder if it doesn't exist
if(!dir.exists("figures")) dir.create("figures")



p1 <- ggplot(content_count, aes(x = Country, y = count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Movie" = "red", "TV Show" = "blue")) +
  labs(title = "Count of Movies and TV Shows by Country",
       x = "Country",
       y = "Count",
       fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/total_media_count_per_country.png", p1,  width = 10, height = 9)



p2 <- ggplot(mov_show_data, aes(x = Genre, y = normalized_duration, fill = Country)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.size = 1) +
  facet_wrap(~ Type, ncol = 1, scales = "free_y") +  # free_y allows each Type its own y-axis
  labs(
    title = "Average Media Duration by Genre and Country",
    x = "Genre",
    y = "Duration (minutes)",
    fill = "Country"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))

ggsave("figures/avg_duration_per_genre_by_country.png", p2,  width = 10, height = 9)

# gene matrix and correlation plotting 
genre_pca <- prcomp(genre_mat |>  select(-Country), scale. = TRUE)

p3 <- fviz_pca_biplot(
  genre_pca,
  geom.ind = "point",
  habillage = genre_mat$Country,
  repel = TRUE
)

ggsave("figures/PCA.png", p3, width = 8, height = 8)

# Correlation plot for genere preferences 

genre_cor <- cor(genre_mat |> select(-Country))

# correlation heatmap
 p4 <- ggplot(melt(genre_cor), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2() +
  theme_minimal()
 
 ggsave("figures/correlation_genre_by_genre_plot.png", p4, width = 8, height = 8)


# Shows/ Movie and the ratings for each 
 

p5 <- ggplot(country_type_summary,
       aes(x = avg_rating, y = reorder(Country, avg_rating), color = Type)) +
  geom_point(size = 4) +
  labs(
    title = "Average Rating of Movies vs TV Shows by Country",
    x = "Average Rating",
    y = "Country"
  ) +
  theme_minimal(base_size = 14)

ggsave("figures/Average_media_rating_country.png", p5, width = 8, height = 8)


p6 <- ggplot(country_genre_summary,
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

ggsave("figures/genre_rating_country.png", p6, width = 8, height = 8)






##============================ User Analysis Plots =============================
# Library Loading
users_data <- readRDS("data/processed/clean_user_data.rds")
age_models <- readRDS("data/processed/age_genre_models.rds")
subscription_model <- readRDS("data/processed/subscription_model.rds")



p7<- ggplot(users_data, aes(x = Favorite_Genre, y = Age, fill = Favorite_Genre)) +
  geom_boxplot() +
  facet_wrap(~ Country) +
  theme_minimal() +
  labs(
    title = "Age Distribution Across Genres Within Countries",
    x = "Favorite Genre",
    y = "Age"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/age_genre_by_country_plot.png", p7, width = 8, height = 5)


p8 <- ggplot(users_data, aes(x = Subscription_Type, y = Watch_Time_Hours, fill = Subscription_Type)) +
  geom_boxplot() +
  facet_wrap(~ Country) +
  theme_minimal() +
  labs(
    title = "Watch Time by Subscription Type Across Countries",
    x = "Subscription Type",
    y = "Watch Time (Hours)"
  )

ggsave("figures/watch_time_by_subscrip_plot.png", p8, width = 8, height = 5)












