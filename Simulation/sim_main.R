setwd("~/RFInterval/sim/")

library(quantregForest)
library(randomForest)
for(i in 1:16)
  source(system("ls ~/RFInterval/sim/conformalInference/*.R", intern = TRUE)[i])

source("RFOOBInterval.R")
source("RFQuanInterval.R")
source("sim_data.R")
source("sim_marg.R")
source("sim_cond.R")

p = 10
rho = 0.6
n0 = 500
nrep = 200
alpha = 0.1
ntree = 2000
x0 <- rbind(rep(0,p), c(1,1,1,rep(0,p-3)), c(2,2,2,rep(0,p-3)))

for(predictor_dist in c("uncorrelated","correlated")){
  for(n in c(200, 500, 1000, 2000, 5000)){  
    for(mean_function in c("linear", "nonlinear", "nonlinear-interaction")){
      for(error_dist in c("homoscedastic", "heavy-tailed", "heteroscedastic")){
        sim_marg_output <- sim_marg(n = n,
                                    p = p,
                                    rho = rho,
                                    x0 = NULL,
                                    n0 = n0,
                                    nrep = nrep,
                                    alpha = alpha,
                                    ntree = ntree,
                                    tune = TRUE,
                                    predictor_dist = predictor_dist,
                                    mean_function = mean_function,
                                    error_dist = error_dist)
        saveRDS(sim_marg_output, paste0("output_tune/sim_marg_", predictor_dist, "_",
                                        mean_function, "_",
                                        error_dist, "_",
                                        n, ".rds"))
      }
    }
  }
}
