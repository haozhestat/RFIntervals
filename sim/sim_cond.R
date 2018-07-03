sim_cond <- function(n = 500,
                     p = 10,
                     rho = 0.6,
                     x0 = rep(0, p),
                     nrep = 200, # Number of repetitions for a given setting
                     alpha = 0.1, # Miscoverage level
                     ntree = 500,
                     tune = FALSE,
                     predictor_dist = c("uncorrelated", "correlated"),
                     mean_function = c("linear", "nonlinear", "nonlinear"),
                     error_dist = c("homoscedastic","heavy-tailed", "heteroscedastic")){
  
  x0 <- matrix(x0, ncol = p)
  
  coverage_conformal_split <-
    coverage_oob_interval <-
    coverage_quantile_interval <-
    length_conformal_split <-
    length_oob_interval <-
    length_quantile_interval <-
    matrix(NA, nrep, nrow(x0))
  
  if(tune){
    cv_tune <- data.frame(nodesize = c(1,1,1,5,5,5),
                          mtry = rep(c(max(ceiling(floor(p/3)/2),1),
                                       max(floor(p/3),1),
                                       max(floor(p/3),1)*2),2),
                          cv_error = rep(0,6))
    for(j in 1:5){
      data <- sim_data(n = (n+n0), p = p, rho = rho, x0 = NULL, predictor_dist = predictor_dist,
                       mean_function = mean_function, error_dist = error_dist)
      x_train <- data$x[1:n,]
      y_train <- data$y[1:n]
      x_test <-  data$x[(n+1):(n+n0),]
      y_test <-  data$y[(n+1):(n+n0)]
      
      for(index in 1:nrow(cv_tune)){
        nodesize = cv_tune$nodesize[index]
        mtry = cv_tune$mtry[index]
        rf = randomForest(x=x_train, y=y_train, ntree=ntree, mtry = mtry, nodesize = nodesize)
        test_pred <- predict(rf, x_test)
        cv_tune$cv_error[index] <- cv_tune$cv_error[index] + mean(abs(y_test - test_pred))
      }
    }
    
    nodesize = cv_tune$nodesize[which.min(cv_tune$cv_error)]
    mtry = cv_tune$mtry[which.min(cv_tune$cv_error)]
    print(c(nodesize, mtry))
  }
  else{
    mtry = if (!is.null(y_train) && !is.factor(y_train))
      max(floor(ncol(x_train)/3), 1) else floor(sqrt(ncol(x_train)))
    nodesize = if (!is.null(y_train) && !is.factor(y_train)) 5 else 1
  }
  
  
  for(k in 1:nrep){
    print(paste0("Start loop ", k, " :"))
    data <- sim_data(n = n, p = 10, rho = 0.6, x0 = x0,
                     predictor_dist = predictor_dist,
                     mean_function = mean_function,
                     error_dist = error_dist)
    
    x_train <- data$x
    y_train <- data$y
    mx <- data$mx
    x_test <- data$x0
    y_test <- data$y0
    mx0 <- data$mx0
    
    my.rf.funs = rf.funs(ntree = ntree, mtry = mtry, nodesize = nodesize)
    
    conformal_split <-  conformal.pred.split(x_train, y_train, x_test,
                                             alpha = alpha, verb= FALSE,
                                             train.fun=my.rf.funs$train,
                                             predict.fun=my.rf.funs$predict)
    
    oob_interval <- RFOOBInterval(x_train, y_train, x_test, alpha = alpha, symmetry = TRUE,
                                  ntree = ntree, mtry=mtry, nodesize=nodesize)
    quantile_interval <- RFQuanInterval(x_train, y_train, x_test, alpha = alpha,
                                        ntree = ntree, mtry=mtry, nodesize=nodesize)
    
    if(error_dist == "homoscedastic"){
      coverage_conformal_split[k,] <-
        pnorm(conformal_split$up - mx0) -pnorm(conformal_split$lo - mx0)
      coverage_oob_interval[k,] <-
        pnorm(oob_interval$up - mx0) -pnorm(oob_interval$lo - mx0)
      coverage_quantile_interval[k,] <-
        pnorm(quantile_interval$up - mx0) -pnorm(quantile_interval$lo - mx0)
    }
    else if(error_dist == "heavy-tailed"){
      coverage_conformal_split[k,] <-
        pt(conformal_split$up - mx0, df = 2) -pt(conformal_split$lo - mx0, df = 2)
      coverage_oob_interval[k,] <-
        pt(oob_interval$up - mx0, df = 2) -pt(oob_interval$lo - mx0, df = 2)
      coverage_quantile_interval[k,] <-
        pt(quantile_interval$up - mx0, df = 2) -pt(quantile_interval$lo - mx0, df = 2)
    }
    else{
      coverage_conformal_split[k,] <-
        pnorm(conformal_split$up - mx0, sd=sqrt(1+abs(mx0)/mean(abs(mx)))) -
        pnorm(conformal_split$lo - mx0, sd=sqrt(1+abs(mx0)/mean(abs(mx))))
      coverage_oob_interval[k,] <-
        pnorm(oob_interval$up - mx0, sd=sqrt(1+abs(mx0)/mean(abs(mx)))) -
        pnorm(oob_interval$lo - mx0, sd=sqrt(1+abs(mx0)/mean(abs(mx))))
      coverage_quantile_interval[k,] <-
        pnorm(quantile_interval$up - mx0, sd=sqrt(1+abs(mx0)/mean(abs(mx)))) -
        pnorm(quantile_interval$lo - mx0, sd=sqrt(1+abs(mx0)/mean(abs(mx))))
    }
    
    
    length_conformal_split[k,] <- conformal_split$up - conformal_split$lo
    length_oob_interval[k,] <- oob_interval$up - oob_interval$lo
    length_quantile_interval[k,] <- quantile_interval$up - quantile_interval$lo
  }
  
  return(list(coverage_oob_interval,
              coverage_conformal_split,
              coverage_quantile_interval,
              length_oob_interval,
              length_conformal_split,
              length_quantile_interval))
}
