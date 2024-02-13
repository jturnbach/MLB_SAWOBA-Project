# Load libraries
library(tidyverse)
library(tidymodels)

# Load data
bip_raw <- read.csv("project/volume/data/interim/bip_raw.csv")

# Create id variable
bip_processed <- bip_raw %>%
  mutate(bip_id = row_number())

# Remove sacrifices and catcher interferences
bip_processed <- bip_processed %>%
  filter(!(events %in% c("sac_bunt", "sac_bunt_double_play", "catcher_interf")))

# Group events
bip_processed <- bip_processed %>%
  mutate(events = case_when(
    events %in% c("field_out", "force_out", "grounded_into_double_play", "field_error", "sac_fly", "double_play", "fielders_choice", "fielders_choice_out", "sac_fly_double_play", "triple_play") ~ "field_out",
    TRUE ~ events
  ))

# remove NA
bip_processed <- bip_processed %>% drop_na()

# remove unneeded variables
bip_processed <- bip_processed %>%
  select(bip_id, launch_speed, launch_angle, spray_angle, sprint_speed, woba_value)

# split data
set.seed(34)
data_split <- initial_split(bip_processed, prop = 0.8)
train <- training(data_split)
test <- testing(data_split)
val <- validation_split(train, prop = 0.2)

# save data for modeling
write.csv(bip_processed, "project/volume/data/processed/bip_processed.csv", row.names = FALSE)
write.csv(train, "project/volume/data/processed/train.csv", row.names = FALSE)
write.csv(test, "project/volume/data/processed/test.csv", row.names = FALSE)
saveRDS(val, "project/volume/data/processed/val.RDS")
