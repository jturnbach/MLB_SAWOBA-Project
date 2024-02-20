# Load libraries
library(tidyverse)

# Read in raw data
batter2020 <- read.csv("project/volume/data/raw/sc_2020.csv")
batter2021 <- read.csv("project/volume/data/raw/sc_2021.csv")
batter2022 <- read.csv("project/volume/data/raw/sc_2022.csv")
batter2023 <- read.csv("project/volume/data/raw/sc_2023.csv")

sprintspeed20 <- read.csv("project/volume/data/raw/sprint_speed_2020.csv")
sprintspeed21 <- read.csv("project/volume/data/raw/sprint_speed_2021.csv")
sprintspeed22 <- read.csv("project/volume/data/raw/sprint_speed_2022.csv")
sprintspeed23 <- read.csv("project/volume/data/raw/sprint_speed_2023.csv")

# Combine pitch by pitch data with sprint speed data
total2020 <- left_join(batter2020, sprintspeed20, by = c("batter" = "player_id"))
total2021 <- left_join(batter2021, sprintspeed21, by = c("batter" = "player_id"))
total2022 <- left_join(batter2022, sprintspeed22, by = c("batter" = "player_id"))
total2023 <- left_join(batter2023, sprintspeed23, by = c("batter" = "player_id"))
master <- rbind(total2020, total2021, total2022, total2023)
master <- master %>% rename(player_id = batter)

# Filter for only balls in play
in_play <- subset(master, bb_type %in% c("ground_ball", "fly_ball", "line_drive", "popup"))

# Estimate spray angle
spray_angle <- with(in_play, round(
  (atan(
    (hc_x-125.42)/(198.27-hc_y)
  )*180/pi*.75)
  ,1))
in_play$spray_angle <- spray_angle

# Form batted ball classification
bip_raw <- in_play %>%
  mutate(bb_class = case_when(
    (launch_speed * 1.5 - launch_angle >= 117) & (launch_speed + launch_angle >= 124) & (launch_speed >= 98) & (launch_angle > 4 & launch_angle < 50) ~ "barrel",
    (launch_speed * 1.5 - launch_angle >= 111) & (launch_speed + launch_angle >= 119) & (launch_speed >= 95) & (launch_angle > 0 & launch_angle < 52) ~ "solid_contact",
    (launch_speed <= 59) ~ "poorly_weak",
    (launch_speed * 2 - launch_angle >= 87) & (launch_angle <= 41) & (launch_speed * 2 + launch_angle <= 175) & (launch_speed + launch_angle * 1.3 >= 89) & (launch_speed > 59 & launch_speed < 72) ~ "flare_burner",
    (launch_speed + launch_angle * 1.3 <= 112) & (launch_speed + launch_angle * 1.55 >= 92) & (launch_speed > 72 & launch_speed < 86) ~ "flare_burner",
    (launch_angle <= 20) & (launch_speed + launch_angle * 2.4 >= 98) & (launch_speed > 86 & launch_speed < 95) ~ "flare_burner",
    (launch_speed - launch_angle >= 76) & (launch_speed + launch_angle * 2.4 >= 98) & (launch_speed >= 95) & (launch_angle <= 30) ~ "flare_burner",
    (launch_speed + launch_angle * 2 >= 116) ~ "poorly_under",
    (launch_speed + launch_angle * 2 <= 116) ~ "poorly_topped",
    TRUE ~ NA_character_))

# Save data for exploration
write.csv(bip_raw, "project/volume/data/interim/bip_raw.csv", row.names = FALSE)
