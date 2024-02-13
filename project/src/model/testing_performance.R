# Load libraries
library(tidyverse)
library(tidymodels)

# Load test set
test <- read.csv("project/volume/data/processed/test.csv")

# Load in final model
final_model <- readRDS("project/volume/models/model_with.rds")

# Generate predictions on held-out test set
test_preds <- predict(final_model, test) %>%
  bind_cols(test)

rmse_res <- rmse(data = test_preds, truth = woba_value, estimate = .pred)

write.csv(rmse_res, paste0("project/volume/models/", model_name, "_rmse.csv"))