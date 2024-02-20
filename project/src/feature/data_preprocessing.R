# Load libraries
library(tidyverse)
library(tidymodels)

# Load data
bip_raw <- read.csv("project/volume/data/interim/bip_raw.csv")

# Create id variable
bip_processed <- bip_raw %>%
  mutate(bip_id = row_number())

# Remove sacrifices and catcher interference
bip_processed <- bip_processed %>%
  filter(!(events %in% c("sac_bunt", "sac_bunt_double_play", "catcher_interf")))

# Create indicator for ground ball. Use for splitting data between KNN and GAM
bip_processed <- bip_processed %>%
  mutate(ground_ball = ifelse(bb_type == "ground_ball", 1, 0))

# remove unneeded variables and remove NA
bip_processed <- bip_processed %>%
  select(bip_id, bb_class, ground_ball, launch_speed, launch_angle, spray_angle, sprint_speed, woba_value) %>%
  drop_na()

# split data
set.seed(40)
data_split <- initial_split(bip_processed, prop = 0.8, strata = woba_value)
train <- training(data_split)
test <- testing(data_split)
val <- validation_split(train, prop = 0.25, strata = woba_value)

# save data for modeling
write.csv(bip_processed, "project/volume/data/processed/bip_processed.csv", row.names = FALSE)
write.csv(train, "project/volume/data/processed/train.csv", row.names = FALSE)
write.csv(test, "project/volume/data/processed/test.csv", row.names = FALSE)
saveRDS(val, "project/volume/data/processed/val.rds")
