#=========
# Data Science Final Project
# Loading in Necessary Data
# Author: Carmen Martin
#===========

# Load libraries
library(dplyr)


# Create processed folder if it doesn't exist
if(!dir.exists("data/processed")) dir.create("data/processed")

# Load raw data
users_data <- read.csv("data/raw/netflix_users.csv")
mov_show_data <- read.csv("data/raw/Netflix_Movies_and_TV_Shows.csv")

head(users_data)
users_data <- users_data |>  select(-User_ID)

head(mov_show_data)
mov_show_data <- mov_show_data |>  select(-Title)
 # cleaning the user data file


clean_user_data <- users_data %>%
  select(-User_ID, -Name, -Last_Login)




# Save filtered data# Save filtered dataTitle
saveRDS(clean_user_data, file = "data/processed/clean_user_data.rds")
saveRDS(mov_show_data, file ="data/processed/mov_show_data.rds")
