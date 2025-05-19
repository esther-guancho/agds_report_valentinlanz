# make model evaluation into a function to reuse code
eval_knn <- function(mod, df_test, df_train){
  
  # add predictions to the data frames
  df_train <- df_train |> 
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  # get metrics tables
  metrics_train <- df_train |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  rmse_train <- metrics_train |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_train <- metrics_train |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  
  mae_train <- metrics_train |> 
    filter(.metric == "mae") |> 
    pull(.estimate)
  mae_test <- metrics_test |> 
    filter(.metric == "mae") |> 
    pull(.estimate)
  
  
  model_metrics <- data.frame(
    Mae_test = mae_test,
    Mae_train = mae_train,
    Rmse_test = rmse_test,
    Rmse_train = rmse_train,
    Rsq_test = rsq_test,
    Rsq_train = rsq_train
  )
  
  out <- list("metrics" = model_metrics, "predict_train" = df_train, 
              "predict_test" = df_test)
  
  return(out)
  
}
