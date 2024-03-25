# Front Matter ----------

# Load libraries
library(tidyverse)
library(tidymodels)
library(kknn)
library(mgcv)

# Load in final models (without spray)
final_knn_with_model <- readRDS("project/volume/models/knn_gam_with_spray_model_knn.rds")
final_gam_with_model <- readRDS("project/volume/models/knn_gam_with_spray_model_gam.rds")

# Read in raw data
batter2020 <- read.csv("project/volume/data/raw/sc_2020.csv")
batter2021 <- read.csv("project/volume/data/raw/sc_2021.csv")
batter2022 <- read.csv("project/volume/data/raw/sc_2022.csv")
batter2023 <- read.csv("project/volume/data/raw/sc_2023.csv")

woba20 <- read.csv("project/volume/data/raw/woba_2020.csv")
woba21 <- read.csv("project/volume/data/raw/woba_2021.csv")
woba22 <- read.csv("project/volume/data/raw/woba_2022.csv")
woba23 <- read.csv("project/volume/data/raw/woba_2023.csv")

# Combine pitch by pitch data with sprint speed data
total2020_woba <- left_join(batter2020, woba20, by = c("batter" = "player_id"))
total2021_woba <- left_join(batter2021, woba21, by = c("batter" = "player_id"))
total2022_woba <- left_join(batter2022, woba22, by = c("batter" = "player_id"))
total2023_woba <- left_join(batter2023, woba23, by = c("batter" = "player_id"))
master_woba <- rbind(total2020_woba, total2021_woba, total2022_woba, total2023_woba)
master_woba <- master_woba %>% rename(player_id = batter)



# Calculate raw totals ----------

# Select only when an event occurs, we don't need every pitch
all_play <- master_woba %>%
  filter(!is.na(events) & events != "")

# Select only columns we need
all_play_clean <- all_play %>%
  select(events, player_id, player_name, game_year, woba_value, woba, est_woba, est_woba_minus_woba_diff)

# Correct wOBA values
all_play_clean <- all_play_clean %>%
  mutate(woba_value = case_when(
    events == "walk" ~ 0.7,
    events == "hit_by_pitch" ~ 0.7,
    events == "single" ~ 0.9,
    events == "double" ~ 1.25,
    events == "triple" ~ 1.6,
    events == "home_run" ~ 2,
    TRUE ~ 0
  ))

# Get walks by player and year and calculates weighted total
walk_hbp <- all_play_clean %>%
  filter(events %in% c("walk", "hit_by_pitch")) %>%
  group_by(player_name, player_id, game_year) %>%
  summarize(
    walk_hbp = sum(events %in% c("walk", "hit_by_pitch")),
    weighted_walk_hbp = walk_hbp * 0.7
  )

all_play_clean <- all_play_clean %>%
  left_join(walk_hbp, by = c("player_id", "game_year", "player_name")) %>%
  mutate(
    walk_hbp = coalesce(walk_hbp, 0),
    weighted_walk_hbp = coalesce(weighted_walk_hbp, 0)
    )

# Calculating denominator of xwOBA 
denom <- all_play_clean %>%
  filter(events %in% c("strikeout", "field_out", "double", "single", 
                       "force_out", "home_run", "triple", "field_error", 
                       "grounded_into_double_play", "fielders_choice", 
                       "fielders_choice_out", "other_out", "triple_play", #AB
                       "walk", #BB
                       "hit_by_pitch", #HBP
                       "sac_fly", "sac_bunt", "sac_fly_double_play", #SF
                       "sac_bunt_double_play")) %>% 
  group_by(player_name, player_id, game_year) %>%
  summarize(total_denom = n())

all_play_clean <- all_play_clean %>%
  left_join(denom, by = c("player_id", "game_year", "player_name"))

# Save data for wOBA calculations
write.csv(all_play_clean, "project/volume/data/processed/woba_all_play.csv", row.names = FALSE)



# Predict xwOBAcon for every batted ball ----------
bbe <- 
  
bbe_fb_ld <-
  
bbe_weaK_gb <-
  

