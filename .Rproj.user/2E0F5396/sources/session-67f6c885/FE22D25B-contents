# =============================
# Data Science Project 
# 
# 
# 
# ============================

library(tidyverse)
library(ggplot2)


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
  scale_fill_manual(values = c("Movie" = "green", "TV Show" = "blue")) +
  labs(title = "Count of Movies and TV Shows by Country",
       x = "Country",
       y = "Count",
       fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(content_sumary, aes(x = Country, y = count, fill = Genre)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Type) +
  labs(
    title = "Most Common Genres by Country and Type",
    x = "Genre",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


top_genre_per_country <- content_sumary |> 
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