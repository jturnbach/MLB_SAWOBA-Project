# Load libraries
library(tidyverse)
library(tidymodels)

# Load processed data
train <- read.csv("project/volume/data/processed/train.csv")
val <- read.csv("project/volume/data/processed/val.csv")

# --- Null Model ---

# Calculate null prediction
null_pred <- mean(train$woba_value)

# Add null predictions to validation data
val_preds <- val %>%
  mutate(.pred = null_pred)

# Calculate null validation error
validation_error <- rmse(data = val_preds, truth = woba_value, estimate = .pred)

# Save null validation performance
write.csv(validation_error, paste0("project/volume/models/null_val_error.csv"))



# --- Naive Model ---

# Calculate naive predictions
naive_predictions <- train %>%
  group_by(bb_class) %>%
  summarise(.pred = mean(woba_value))

# Add naive predictions to validation data
val_preds <- val %>%
  left_join(naive_predictions, by = "bb_class")

# Calculate naive validation error
validation_error <- rmse(data = val_preds, truth = woba_value, estimate = .pred)

# Save naive validation performance
write.csv(validation_error, paste0("project/volume/models/naive_val_error.csv"))
