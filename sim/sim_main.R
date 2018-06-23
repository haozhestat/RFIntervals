setwd("~/Documents/RFInterval/sim/")

#library(devtools)
#install_github(repo="ryantibs/conformal", subdir="conformalInference")
library(randomForest)
library(conformalInference)
#for(i in 1:16)
#  source(system("ls ~/RFInterval/sim/conformalInference/*.R", intern = TRUE)[i])
my.rf.funs = rf.funs()
library(quantregForest)
#library(randomForestCI)

source("RFOOBInterval.R")
source("RFQuanInterval.R")
source("sim_data.R")
source("sim_marg.R")
source("sim_cond.R")

n = 500
p = 10
rho = 0.6
n0 = 200
nrep = 200
alpha = 0.1
ntree = 500
x0 <- rbind(rep(0,p), rep(1,p), rep(-1,p),rep(2,p), rep(-2,p))

for(predictor_dist in c("uncorrelated", "correlated")){
  for(mean_function in c("linear", "nonlinear", "nonlinear")){
    for(error_dist in c("homoscedastic","heavy-tailed", "heteroscedastic")){
      sim_marg_output <- sim_marg(n = n, 
                                  p = p, 
                                  rho = rho, 
                                  x0 = NULL, 
                                  n0 = n0, 
                                  nrep = nrep, 
                                  alpha = alpha, 
                                  ntree = ntree, 
                                  predictor_dist = predictor_dist, 
                                  mean_function = mean_function, 
                                  error_dist = error_dist)
      
      saveRDS(sim_marg_output, paste0("sim_marg_", predictor_dist, "_", 
                                      mean_function, "_",
                                      error_dist, "_",
                                      alpha, ".rds"))
      
      sim_cond_output <- sim_cond(n = n, 
                                  p = p, 
                                  rho = rho, 
                                  x0 = x0, 
                                  nrep = nrep, 
                                  alpha = alpha, 
                                  ntree = ntree, 
                                  predictor_dist = predictor_dist, 
                                  mean_function = mean_function, 
                                  error_dist = error_dist)
      saveRDS(sim_cond_output, paste0("sim_cond_", predictor_dist, "_", 
                                      mean_function, "_",
                                      error_dist, "_",
                                      alpha, ".rds"))
    }
  }
}


