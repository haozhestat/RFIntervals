RFOOBInterval <- function(x, 
                          y, 
                          x0, 
                          ntree = 1000, 
                          alpha = 0.10, 
                          symmetry = TRUE,
                          mtry = if (!is.null(y) && !is.factor(y))
                            max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))), 
                          nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1){
  x <- as.matrix(x)
  x0 <- as.matrix(x0)
  colnames(x) <- 1:ncol(x)
  rownames(x) <- 1:nrow(x)
  colnames(x0) <- 1:ncol(x0)
  rownames(x0) <- 1:nrow(x0)
  
  n = nrow(x)
  ntest = nrow(x0)
  
  rf = randomForest(x=x, y=y, ntree=ntree, mtry = mtry, nodesize = nodesize,
                    keep.forest=TRUE, keep.inbag=TRUE)
  
  test_pred <- predict(rf, x0)
  oob_abs_error = sort(abs(y - rf$predicted))
  oob_error = sort(y - rf$predicted)
  
  upper_pred = rep(NA, ntest)
  lower_pred = rep(NA, ntest)
  
  ## symmetry = TRUE leads to the symmetric OOB Intervals
  ## symmetry = FALSE leads to the standard OOB Intervals
  if(symmetry){
    for (i in 1:ntest){
      upper_pred[i] = test_pred[i] + quantile(oob_abs_error,1-alpha)
      lower_pred[i] = test_pred[i] - quantile(oob_abs_error,1-alpha)
    }
  }
  else{
    for (i in 1:ntest){
      upper_pred[i] = test_pred[i] + quantile(oob_error, 1-alpha/2)
      lower_pred[i] = test_pred[i] + quantile(oob_error, alpha/2)
    }
    
  }
  
  return(list(pred = matrix(test_pred,ntest,1),
              lo = matrix(lower_pred,ntest,1), 
              up = matrix(upper_pred,ntest,1), 
              fit = matrix(predict(rf,x),n,1)))
}
