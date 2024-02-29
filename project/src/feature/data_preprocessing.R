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

# remove unneeded variables and remove NA
bip_processed <- bip_processed %>%
  select(bip_id, bb_class, launch_speed, launch_angle, spray_angle, sprint_speed, woba_value) %>%
  drop_na()

# split data
set.seed(3)
data_split <- initial_validation_split(bip_processed, prop = c(0.6, 0.2), strata = woba_value)
train <- training(data_split)
val <- validation(data_split)
val_set <- validation_set(data_split)
test <- testing(data_split)

# save data for modeling
write.csv(bip_processed, "project/volume/data/processed/bip_processed.csv", row.names = FALSE)
write.csv(train, "project/volume/data/processed/train.csv", row.names = FALSE)
write.csv(val, "project/volume/data/processed/val.csv", row.names = FALSE)
saveRDS(val_set, "project/volume/data/processed/val_set.rds")
write.csv(test, "project/volume/data/processed/test.csv", row.names = FALSE)
