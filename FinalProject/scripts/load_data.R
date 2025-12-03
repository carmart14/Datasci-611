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
users_data <- read.csv("project/data/raw/netflix_users.csv")
mov_show_data <- read.csv("project/data/raw/Netflix_Movies_and_TV_Shows.csv")

head(users_data)
clean_user_data <- users_data |>  select(-User_ID,  -Name, -Last_Login)

head(mov_show_data)
mov_show_data <- mov_show_data |>  select(-Title)
 # cleaning the user data file


# Save filtered data# Save filtered dataTitle
saveRDS(clean_user_data, file = "project/data/processed/clean_user_data.rds")
saveRDS(mov_show_data, file ="project//data/processed/mov_show_data.rds")


rm(users_data)

