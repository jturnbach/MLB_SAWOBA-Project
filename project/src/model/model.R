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
val <- read.csv("project/volume/data/processed/val.csv")
val_set <- readRDS("project/volume/data/processed/val_set.rds")

# Define function to train models using tidymodels framework
train_xwobacon_model <- function(train, val, model_type, spray, model_name) {
  
  if (spray == "no") {
    train <- train %>% select(-spray_angle)
    val <- val %>% select(-spray_angle)
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
    
    num_predictors <- ncol(select(train, -bip_id, -bb_class, -woba_value))
    mtry_param <- mtry(range = c(1, num_predictors))
    
    params <- parameters(spec) %>% update(mtry = mtry_param)
    
  } else if (model_type == "knn") {
    
    spec <- nearest_neighbor(neighbors = tune(), weight_func = "rectangular", dist_power = 2) %>%
      set_mode("regression") %>%
      set_engine("kknn")
    
    params <- grid_regular(neighbors(range = c(100, 600)), levels = 6)
    
  } else if (model_type == "knn_gam") {
    train_val <- train %>% rbind(val)
    
    # KNN for majority of batted balls
    train_val_knn <- train_val %>% filter(!(bb_class %in% c("poorly_weak", "poorly_topped"))) %>% select(-sprint_speed)
    
    set.seed(3)
    data_split_knn <- initial_validation_split(train_val_knn, prop = c(0.6, 0.2), strata = woba_value)
    train_knn <- training(data_split_knn)
    val_knn <- validation(data_split_knn)
    val_set_knn <- validation_set(data_split_knn)
    #test_knn <- testing(data_split_knn)
    
    spec_knn <- nearest_neighbor(neighbors = tune(), weight_func = "rectangular", dist_power = 2) %>%
      set_mode("regression") %>%
      set_engine("kknn")
    
    params_knn <- grid_regular(neighbors(range = c(100, 600)), levels = 6)
    
    # GAM for weak/topped balls
    train_val_gam <- train_val %>% filter(bb_class %in% c("poorly_weak", "poorly_topped"))
    
    set.seed(3)
    data_split_gam <- initial_validation_split(train_val_gam, prop = c(0.6, 0.2), strata = woba_value)
    train_gam <- training(data_split_gam)
    val_gam <- validation(data_split_gam)
    #val_set_gam <- validation_set(data_split_gam)
    #test_gam <- testing(data_split_gam)
  }
  
  # Create model recipe
  if (model_type == "knn") {
    rec <- recipe(woba_value ~ ., data = train) %>%
      update_role(bip_id, bb_class, new_role = "ID") %>%
      step_normalize(all_predictors(), role = "predictor")
    
  } else if (model_type == "knn_gam") {
    rec_knn <- recipe(woba_value ~ ., data = train_knn) %>%
      update_role(bip_id, bb_class, new_role = "ID") %>%
      step_normalize(all_predictors(), role = "predictor")
    
  } else {
    rec <- recipe(woba_value ~ ., data = train) %>%
      update_role(bip_id, bb_class, new_role = "ID")
  }
  
  # Combine specification and recipe into a workflow
  if (model_type != "knn_gam") {
    wf <- workflow() %>%
      add_model(spec) %>%
      add_recipe(rec)
  } else {
    wf_knn <- workflow() %>%
      add_model(spec_knn) %>%
      add_recipe(rec_knn)
  }
  
  # Tune hyperparameters via Bayesian optimization using validation set
  if (!(model_type %in% c("knn", "knn_gam"))) {
    
    tune_res <- wf %>%
      tune_bayes(resamples = val_set,
                 initial = 10,
                 iter = 100,
                 param_info = params,
                 control = control_bayes(verbose = TRUE, no_improve = 10, seed = 40),
                 metrics = metric_set(rmse))
    
  } else if (model_type == "knn") {
    
    tune_res <- wf %>%
      tune_grid(resamples = val_set,
                grid = params,
                control = control_grid(verbose = TRUE),
                metrics = metric_set(rmse))
    
  } else if (model_type == "knn_gam") {
    
    tune_res_knn <- wf_knn %>%
      tune_grid(resamples = val_set_knn,
                grid = params_knn,
                control = control_grid(verbose = TRUE),
                metrics = metric_set(rmse))
    
    if (spray == "no") {
      gam_model <- gam(woba_value ~ sprint_speed + te(launch_speed, launch_angle), data = train_gam)
    } else if (spray == "yes") {
      gam_model <- gam(woba_value ~ sprint_speed + te(launch_speed, launch_angle, spray_angle), data = train_gam)
    }
    
    val_preds_gam <- predict(gam_model, val_gam)
    val_preds_gam <- cbind(val_gam, .pred = val_preds_gam)
  }
  
  # Collect validation errors
  if (model_type == "knn_gam") {
    validation_error_knn <- tune_res_knn %>%
      collect_metrics() %>%
      filter(.metric == "rmse")
    
    validation_error_gam <- rmse(val_preds_gam, truth = woba_value, estimate = .pred)
    
    best_validation_error_knn <- validation_error_knn %>% filter(mean == min(mean))
    validation_error <- (nrow(val_knn) * best_validation_error_knn[["mean"]] + nrow(val_gam) * validation_error_gam[[".estimate"]]) / (nrow(val_knn) + nrow(val_gam))
    
  } else {
    validation_error <- tune_res %>%
      collect_metrics() %>%
      filter(.metric == "rmse")
  }
  
  # Save validation performances, including best one
  write.csv(validation_error, paste0("project/volume/models/", model_name, "_val_error.csv"), row.names = FALSE)
  
  # Extract best-performing hyperparameters
  if (model_type == "knn_gam") {
    best_params_knn <- select_best(tune_res_knn, "rmse")
    
    # Combine training and validation to train final model
    train_knn <- rbind(train_knn, val_knn)
    
    # Train best candidate model
    set.seed(40)
    final_model_knn <- wf_knn %>%
      finalize_workflow(best_params_knn) %>%
      fit(data = train_knn)
    
    # Save best model
    saveRDS(final_model_knn, paste0("project/volume/models/", model_name, "_knn.rds"))
    
    # Combine training and validation to train final model
    train_gam <- rbind(train_gam, val_gam)
    
    if (spray == "no") {
      final_model_gam <- gam(woba_value ~ sprint_speed + te(launch_speed, launch_angle), data = train_gam)
    } else if (spray == "yes") {
      final_model_gam <- gam(woba_value ~ sprint_speed + te(launch_speed, launch_angle, spray_angle), data = train_gam)
    }
    
    # Save best model
    saveRDS(final_model_gam, paste0("project/volume/models/", model_name, "_gam.rds"))
    
  } else {
    best_params <- select_best(tune_res, "rmse")
    
    # Combine training and validation to train final model
    train <- rbind(train, val)
    
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