sim_data <- function(n = 500,
                    p = 10,
                    rho = 0.6,
                    x0 = NULL,
                    predictor_dist = c("uncorrelated", "correlated"),
                    mean_function = c("linear", "nonlinear", "nonlinear-interaction"),
                    error_dist = c("homoscedastic","heavy-tailed", "heteroscedastic","skewed")){
  
  if(predictor_dist == "uncorrelated")
    x <- matrix(rnorm(n*p, 0, 1), n, p)
  else{
    library(MASS)
    Sigma <- diag(rep(1,p))
    for(i in 1:p){
      for(j in i:p){
        Sigma[i,j] <- Sigma[j,i] <- rho^(abs(i-j))
      }
    }
    x <- mvrnorm(n, mu = rep(0,p), Sigma = Sigma)
  }
    
  if(mean_function == "linear")
    mx <- x[,1] + x[,2] 
  else if(mean_function == "nonlinear")
    mx <- exp(-abs(x[,1])-abs(x[,2]))
  else
    mx <- exp(-abs(x[,1])-abs(x[,2])) + x[,1]*x[,2] 
 
  if(error_dist == "homoscedastic")
    epsilon <- rnorm(n, mean = 0, sd = 1)
  else if(error_dist == "heavy-tailed")
    epsilon <- rt(n, df = 2)
  else if(error_dist == "heteroscedastic")
    epsilon <- rnorm(n, mean = 0, sd = sqrt(1+abs(mx)/mean(abs(mx))))
  #else if(error_dist == "skewed")
  #  epsilon <- exp(rnorm(n, mean = 0,sd = sqrt(0.5))) - exp(0.25)

  if(!is.null(x0)){
    x0 <- matrix(x0, ncol = p)
    
    if(mean_function == "linear")
      mx0 <- x0[,1] + x0[,2] 
    else if(mean_function == "nonlinear")
      mx0 <- exp(-abs(x0[,1])-abs(x0[,2]))
    else
      mx0 <- exp(-abs(x0[,1])-abs(x0[,2])) + x0[,1]*x0[,2]      
    
    if(error_dist == "homoscedastic")
      epsilon0 <- rnorm(nrow(x0), mean = 0, sd = sqrt(1))
    else if(error_dist == "heavy-tailed")
      epsilon0 <- rt(nrow(x0), df = 2)
    else
      epsilon0 <- rnorm(nrow(x0), mean = 0, sd = sqrt(1+abs(mx0)/mean(abs(mx))))
    
    return(list(x = x, y = mx + epsilon, mx = mx, x0 = x0, y0 = mx0 + epsilon0, mx0 = mx0))
  }
  
  return(list(x = x, y = mx + epsilon))
}
