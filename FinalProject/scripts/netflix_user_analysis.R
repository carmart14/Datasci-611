# =============================
# Data Science Project 
# 
# 
# 
# ============================


data_movsho <- read.csv(file = "/Users/carmart/Downloads/DS611/FinalProject/Netflx_Movies_TV/Netflix_Movies_and_TV_Shows.csv")


# ========== data user information analysis =========

head(data_user)

data_user |> 
  group_by(Country) |> 
  reframe(count = n())

# brazil, france, mexico, south korea are the non shared countries 












