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

# Save data for exploration
write.csv(in_play, "project/volume/data/interim/bip_raw.csv", row.names = FALSE)
