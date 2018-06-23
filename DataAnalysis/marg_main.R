#setwd("/Users/haozhe/Documents/RFInterval/DataAnalysis/")
setwd("~/RFInterval/DataAnalysis/")

#library(devtools)
#install_github(repo="ryantibs/conformal", subdir="conformalInference")
library(randomForest)
library(quantregForest)
library(caret)
#library(conformalInference)
for(i in 1:16)
  source(system("ls ~/RFInterval/sim/conformalInference/*.R", intern = TRUE)[i])
  #source(system("ls /Users/haozhe/Documents/RFInterval/sim/conformalInference/*.R", intern = TRUE)[i])
my.rf.funs = rf.funs()
#library(randomForestCI)

source("RFOOBInterval.R")
source("RFQuanInterval.R")
#source("sim_data.R")
#source("sim_marg.R")
#source("sim_cond.R")

datalist <- system('ls nipsdata/data/', intern=T)

alpha <- 0.1
nrep <- n_rep <- 50

coverage_conformal_split <- 
  coverage_oob_interval <-
  coverage_quantile_interval <-
  length_conformal_split <- 
  length_oob_interval <- 
  length_quantile_interval <- 
  rep(NA, nrep)

for(i in 21:length(datalist)){
  x <- as.matrix(read.table(paste0('nipsdata/data/',datalist[i],'/x.txt'),head=F))
  y <- scan(paste0('nipsdata/data/',datalist[i],'/y.txt'),0)
  
  partitions <- createFolds(1:length(y), k = 2, list = TRUE, returnTrain = TRUE)
  for(k in 1:n_rep){
      x_train = x[partitions[[1]],]
      y_train = y[partitions[[1]]]
      x_test = x[partitions[[2]],]
      y_test = y[partitions[[2]]]
      
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
      
      print(paste0(datalist[i], "(", i, ")"))
      print(paste0("OOB: ", coverage_oob_interval[k], ";",
                   "Conformal: ", coverage_conformal_split[k], ";",
                   "Quantile: ", coverage_quantile_interval[k], ";"))
    }
  data_result <- list(coverage_oob_interval = coverage_oob_interval,
                      coverage_conformal_split = coverage_conformal_split,
                      coverage_quantile_interval = coverage_quantile_interval,
                      length_oob_interval = length_oob_interval,
                      length_conformal_split = length_conformal_split,
                      length_quantile_interval = length_quantile_interval)
  
  saveRDS(data_result, file=paste0("output/marginprob_coverage/",datalist[i], "_marginprob.rds"))
}






