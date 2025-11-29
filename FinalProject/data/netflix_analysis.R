# =============================
# Data Science Project 
# 
# 
# 
# ============================

library(tidyverse)
library(ggplot2)
library(dplyr)


data_user <- read.csv(file = "/Users/carmart/Downloads/DS611/FinalProject/Netflix_UserInfo/netflix_users.csv")

data_movsho <- read.csv(file = "/Users/carmart/Downloads/DS611/FinalProject/Netflx_Movies_TV/Netflix_Movies_and_TV_Shows.csv")


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
  geom_col(position = "dodge",) +
  facet_wrap(~ Country, scales = "free_x") +
  labs(title = "Genre Preference Proportions by Country",
       y = "Proportion")

# prefernce scoring 

pref_score <- content_sumary |> 
  pivot_wider(names_from = Type, values_from = count, values_fill = 0) |> 
  mutate(
    preference = (Movie - `TV Show`) / (Movie + `TV Show`) # keep the backticks to account for the space char
  )

ggplot(pref_score, aes(x = Country, y = Genre, fill = preference)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  labs(title = "Movie vs TV Genre Preference Score")

























top_genre_per_cCountrytop_genre_per_country <- content_sumary |> 
  group_by(Country, Type) |> 
  slice_max(count, n = 2)


ggplot(top_genre_per_country, aes(x = Country, y = Type, fill = Genre)) +
  geom_tile(color = "white") +
  labs(
    title = "Most Prominent Genre per Country and Type",
    x = "Country",
    y = "Type"
  ) +
  theme_minimal()


data_movsho |> 
  group_by(Type, Country, Genre, Rating) |> 
  summarise(count = n()) |> 
  arrange(desc(count))


# =========== MovShow Analysis ======================

movshow_anova <- 




# ========== data user information analysis =========

head(data_user)

data_user |> 
  group_by(Country) |> 
  reframe(count = n())

# brazil, france, mexico, south korea are the non shared countries 