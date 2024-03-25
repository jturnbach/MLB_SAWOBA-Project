# Load libraries
library(tidyverse)
library(tidymodels)
library(kknn)
library(mgcv)

# Load test set
test <- read.csv("project/volume/data/processed/test.csv")

# Split test by batted ball type
test1 <- test %>% filter(!(bb_class %in% c("poorly_weak", "poorly_topped")))
test2 <- test %>% filter(bb_class %in% c("poorly_weak", "poorly_topped"))

# Load in final model
final_knn_without_model <- readRDS("project/volume/models/knn_gam_without_spray_model_knn.rds")
final_gam_without_model <- readRDS("project/volume/models/knn_gam_without_spray_model_gam.rds")

final_knn_with_model <- readRDS("project/volume/models/knn_gam_with_spray_model_knn.rds")
final_gam_with_model <- readRDS("project/volume/models/knn_gam_with_spray_model_gam.rds")

# Generate predictions on held-out test set (Without Spray)
knn_without_test_preds <- predict(final_knn_without_model, test1) %>%
  bind_cols(test1)

gam_without_test_preds <- predict(final_gam_without_model, test2)
gam_without_test_preds <- cbind(test2, .pred = gam_without_test_preds)

without_test_preds <- rbind(knn_without_test_preds, gam_without_test_preds)
without_rmse <- rmse(data = without_test_preds, truth = woba_value, estimate = .pred)
write.csv(without_rmse, paste0("project/volume/models/knn_gam_without_spray_testing_rmse.csv"))

# Generate predictions on held-out test set (With Spray)
knn_with_test_preds <- predict(final_knn_with_model, test1) %>%
  bind_cols(test1)

gam_with_test_preds <- predict(final_gam_with_model, test2)
gam_with_test_preds <- cbind(test2, .pred = gam_with_test_preds)

with_test_preds <- rbind(knn_with_test_preds, gam_with_test_preds)
with_rmse <- rmse(data = with_test_preds, truth = woba_value, estimate = .pred)
write.csv(with_rmse, paste0("project/volume/models/knn_gam_with_spray_testing_rmse.csv"))