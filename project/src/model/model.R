# Load libraries
library(tidyverse)
library(tidymodels)
library(xgboost)
library(ranger)
library(kknn)
library(mgcv)
library(vip)

# Load processed data
train <- read.csv("project/volume/data/processed/train.csv")
val <- readRDS("project/volume/data/processed/val.rds")

# Define function to train models using tidymodels framework
train_xwobacon_model <- function(train, val, model_type, spray, model_name) {
  
  if (spray == "no") {
    train <- train %>% select(-spray_angle)
  }
  
  # Create model specification based on model type
  if (model_type == "gbt") {
    spec <- boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), sample_size = tune()) %>%
      set_mode("regression") %>%
      set_engine("xgboost")
    
    params <- parameters(spec)
    
  } else if (model_type == "rf") {
    
    spec <- rand_forest(trees = tune(), min_n = tune(), mtry = tune()) %>%
      set_mode("regression") %>%
      set_engine("ranger", importance = "impurity")
    
    num_predictors <- ncol(select(train, -bip_id, -woba_value))
    mtry_param <- mtry(range = c(1, num_predictors))
    
    params <- parameters(spec) %>%
      update(mtry = mtry_param)
    
  } else if (model_type == "knn") {
    spec <- nearest_neighbor(neighbors = tune(), weight_func = "rectangular", dist_power = 2) %>%
      set_mode("regression") %>%
      set_engine("kknn")
    
    params <- parameters(spec)
  } else if (model_type == "knn_gam") {
    
    validation <- analysis(val$splits[[1]])
    
    # split by bb_type
    
  }
  
  # Create model recipe
#  if (model_type == "knn") {
 #   # scale features
#  } else if {
    #rec <- recipe(woba_value ~ ., data = train) %>%
   #   update_role(bip_id, new_role = "ID")
  #}
  
  # Combine specification and recipe into a workflow
  wf <- workflow() %>%
    add_model(spec) %>%
    add_recipe(rec)
  
  # Tune hyperparameters via Bayesian optimization using validation set
  tune_res <- wf %>%
    tune_bayes(resamples = val,
               initial = 10,
               iter = 100,
               param_info = params,
               control = control_bayes(verbose = TRUE, no_improve = 10, seed = 40),
               metrics = metric_set(rmse))
  
  # Collect validation errors
  validation_error <- tune_res %>%
    collect_metrics() %>%
    filter(.metric == "rmse")
  
  # Save validation performances, including best one
  write.csv(validation_error, paste0("project/volume/models/", model_name, "_val_error.csv"))
  
  # Extract best-performing hyperparameters
  best_params <- select_best(tune_res, "rmse")
  
  # Train best candidate model
  set.seed(40)
  final_model <- wf %>%
    finalize_workflow(best_params) %>%
    fit(data = train)
  
  # Save best model
  saveRDS(final_model, paste0("project/volume/models/", model_name, ".rds"))
  
  # Generate variable importance plot
  if (model_type %in% c("gbt", "rf")) {
    vip_plot <- final_model %>% 
      extract_fit_parsnip() %>% 
      vip() +
      ggtitle("Feature Importance")
    
  # Save VIP plot
  ggsave(filename = paste0("project/volume/plots/", model_name, "_vip.png"), plot = vip_plot, width = 10, height = 8, dpi = 300)
  }
  
}

# Use function to train models
train_xwobacon_model(train, val, spray = "no", model_type = "gbt", model_name = "gbt_without_spray_model")
train_xwobacon_model(train, val, spray = "yes", model_type = "gbt", model_name = "gbt_with_spray_model")
train_xwobacon_model(train, val, spray = "no", model_type = "rf", model_name = "rf_without_spray_model")
train_xwobacon_model(train, val, spray = "yes", model_type = "rf", model_name = "rf_with_spray_model")
train_xwobacon_model(train, val, spray = "no", model_type = "knn", model_name = "knn_without_spray_model")
train_xwobacon_model(train, val, spray = "yes", model_type = "knn", model_name = "knn_with_spray_model")
train_xwobacon_model(train, val, spray = "no", model_type = "knn_gam", model_name = "knn_gam_without_spray_model")
train_xwobacon_model(train, val, spray = "yes", model_type = "knn_gam", model_name = "knn_gam_with_spray_model")