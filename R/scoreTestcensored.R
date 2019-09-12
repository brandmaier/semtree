scoreTestcensored <- function(fit, data_sorted, covariate_sorted, level, test,
                              parameter = NULL, alpha, min_bucket,
                              bin_control) {
  
  # Repeat score test for with censored data set on the left side
  if (bin_control$small_bin == "left" & bin_control$censored_left == FALSE) {
    
    bin_control$censored_left <- TRUE
    
    data_sorted <- data_sorted[-c(1:min_bucket), ]
    covariate_sorted <- covariate_sorted[-c(1:min_bucket)] 
    fit <- mxModel(model = fit,
                   mxData(observed = data_sorted, type = "raw"))
    fit <- mxRun(model = fit, silent = TRUE)
    
    test_result <- scoretest(fit = fit,
                             data_sorted = data_sorted,
                             covariate_sorted = covariate_sorted,
                             level = level,
                             test = test,
                             alpha = alpha,
                             min_bucket = min_bucket,
                             bin_control = bin_control)
    
  }
  
  # Repeat score test for with censored data set on the right side
  if (bin_control$small_bin == "right" & bin_control$censored_right == FALSE) {
    
    bin_control$censored_right <- TRUE
    
    N <- nrow(data_sorted)
    data_sorted <- data_sorted[-c((N-min_bucket+1):N), ]
    covariate_sorted <- covariate_sorted[-c((N-min_bucket+1):N)] 
    fit <- mxModel(model = fit,
                   mxData(observed = data_sorted, type = "raw"))
    fit <- mxRun(model = fit, silent = TRUE)
    
    test_result <- scoretest(fit = fit,
                             data_sorted = data_sorted,
                             covariate_sorted = covariate_sorted,
                             level = level,
                             test = test,
                             alpha = alpha,
                             min_bucket = min_bucket,
                             bin_control = bin_control)
  }
  
  output <- list(data_sorted = data_sorted,
                 covariate_sorted = covariate_sorted,
                 test_result = test_result)
  
  return(output)
  
}
