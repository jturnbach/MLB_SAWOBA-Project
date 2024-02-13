train_gbt_model <- function(train, test, val, model_name) {
  
  spec <- boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), sample_size = tune()) %>%
    set_mode("regression") %>%
    set_engine("xgboost")
  
  rec <- recipe(woba_value ~ ., data = train) %>%
    update_role(bip_id, new_role = "ID")
  
  wf <- workflow() %>%
    add_model(spec) %>%
    add_recipe(rec)
  
  tune_res <- wf %>%
    tune_bayes(resamples = folds,
               initial = 10,
               iter = 100,
               control = control_bayes(verbose = TRUE, no_improve = 10, seed = 34),
               metrics = metric_set(rmse))
  
  best_params <- select_best(tune_res, "rmse")
  
  set.seed(34)
  final_model <- wf %>%
    finalize_workflow(best_params) %>%
    fit(data = train)
  
  saveRDS(final_model, paste0("project/volume/models/", model_name, ".rds"))
  
  test_preds <- predict(final_model, test) %>%
    bind_cols(test)
  
  rmse_res <- rmse(data = preds, truth = woba_value, estimate = .pred)
  
  write.csv(rmse_res, paste0("project/volume/models/", model_name, "_rmse.csv"))
  
  vip_plot <- final_model %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 40) +
    ggtitle("Feature Importance")
  
  # Save feature importance plot
  ggsave(filename = paste0("project/volume/models/", model_name, "_vip.png"), plot = vip_plot, width = 10, height = 8, dpi = 300)
  
}