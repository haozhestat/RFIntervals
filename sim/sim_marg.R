sim_marg <- function(n = 500,
                     p = 10,
                     rho = 0.6,
                     x0 = NULL,
                     n0 = 200, # Number of points at which to make predictions
                     nrep = 200, # Number of repetitions for a given setting
                     alpha = 0.1, # Miscoverage level
                     ntree = 500,
                     predictor_dist = c("uncorrelated", "correlated"),
                     mean_function = c("linear", "nonlinear", "nonlinear"),
                     error_dist = c("homoscedastic","heavy-tailed", "heteroscedastic")){
  
  coverage_conformal_split <- 
    coverage_oob_interval <-
    coverage_quantile_interval <-
    length_conformal_split <- 
    length_oob_interval <- 
    length_quantile_interval <- 
    rep(NA, nrep)
  
  for(k in 1:nrep){
    print(paste0("Start loop ", k, " :"))
    data <- sim_data(n = (n+n0), p = 10, rho = 0.6, x0 = NULL, predictor_dist = predictor_dist,
                         mean_function = mean_function, error_dist = error_dist)
    x_train <- data$x[1:n,]
    y_train <- data$y[1:n]
    x_test <-  data$x[(n+1):(n+n0),]
    y_test <-  data$y[(n+1):(n+n0)]

    conformal_split <-  conformal.pred.split(x_train, y_train, x_test,
                                             alpha = alpha, verb= FALSE,
                                             train.fun=my.rf.funs$train,
                                             predict.fun=my.rf.funs$predict)
    
    oob_interval <- RFOOBInterval(x_train, y_train, x_test, alpha = alpha, symmetry = TRUE)
    quantile_interval <- RFQuanInterval(x_train, y_train, x_test, alpha = alpha)
    
    coverage_conformal_split[k] <- mean(conformal_split$lo < y_test & conformal_split$up > y_test)
    coverage_oob_interval[k] <- mean(oob_interval$lo < y_test & oob_interval$up > y_test)
    coverage_quantile_interval[k] <- mean(quantile_interval$lo < y_test & quantile_interval$up > y_test)
    length_conformal_split[k] <- mean(conformal_split$up - conformal_split$lo )
    length_oob_interval[k] <- mean(oob_interval$up - oob_interval$lo)
    length_quantile_interval[k] <- mean(quantile_interval$up - quantile_interval$lo)
    
    print(paste0("OOB: ", coverage_oob_interval[k], ";",
                 "Conformal: ", coverage_conformal_split[k], ";",
                 "Quantile: ", coverage_quantile_interval[k], ";"))
  }
  
  return(list(coverage_oob_interval,
              coverage_conformal_split,
              coverage_quantile_interval,
              length_oob_interval,
              length_conformal_split,
              length_quantile_interval))
}


  

