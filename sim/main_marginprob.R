setwd("/Users/haozhe/Documents/RFInterval/DataAnalysis/")

library(randomForest)
library(quantregForest)
library(conformalInference)
library(caret)
my.rf.funs = rf.funs()

source("RF00BInterval.R")
source("RFQuanInterval.R")

datalist <- system('ls nipsdata/data/',intern=T)

alpha <- 0.1
n_cv <- 5
n_rep <- 10
coverage_conformal_split <- matrix(NA, n_rep, n_cv)
coverage_oob_interval <- matrix(NA, n_rep, n_cv)
coverage_quantile_interval <- matrix(NA, n_rep, n_cv)
length_conformal_split <- matrix(NA, n_rep, n_cv)
length_oob_interval <- matrix(NA, n_rep, n_cv)
length_quantile_interval <- matrix(NA, n_rep, n_cv)

for(i in 1:length(datalist)){
  x <- as.matrix(read.table(paste0('nipsdata/data/',datalist[i],'/x.txt'),head=F))
  y <- scan(paste0('nipsdata/data/',datalist[i],'/y.txt'),0)
  
  include_index <- matrix(0, n_rep*n_cv, length(y))
  cover_mat_conformal_split <- matrix(FALSE, n_rep*n_cv, length(y))
  cover_mat_oob_interval <- matrix(FALSE, n_rep*n_cv, length(y))
  cover_mat_quantile_interval <- matrix(FALSE, n_rep*n_cv, length(y))
  
  partitions <- createFolds(1:length(y), k = 5, list = TRUE, returnTrain = TRUE)
  for(k in 1:n_rep){
    for(j in 1:n_cv){
      x_train = x[partitions[[j]],]
      y_train = y[partitions[[j]]]
      x_test = x[setdiff(1:length(y), partitions[[j]]),]
      y_test = y[setdiff(1:length(y), partitions[[j]])]
      
      conformal_split <-  conformal.pred.split(x_train, y_train, x_test,
                                               alpha = alpha, verb= FALSE,
                                               train.fun=my.rf.funs$train,
                                               predict.fun=my.rf.funs$predict)
      
      oob_interval <- RFOOBInterval(x_train, y_train, x_test, alpha = alpha, symmetry = TRUE)
      quantile_interval <- RFQuanInterval(x_train, y_train, x_test, alpha = alpha)
      
      include_index[(k-1)*n_cv+j, partitions[[j]]] <- 1
      cover_mat_conformal_split[(k-1)*n_cv+j,setdiff(1:length(y), partitions[[j]])] <-
        conformal_split$lo < y_test & conformal_split$up > y_test
      cover_mat_oob_interval[(k-1)*n_cv+j,setdiff(1:length(y), partitions[[j]])] <-
        oob_interval$lo < y_test & oob_interval$up > y_test
      cover_mat_quantile_interval[(k-1)*n_cv+j,setdiff(1:length(y), partitions[[j]])] <-
        quantile_interval$lo < y_test & quantile_interval$up > y_test
      
      coverage_conformal_split[k,j] <- mean(conformal_split$lo < y_test & conformal_split$up > y_test)
      coverage_oob_interval[k,j] <- mean(oob_interval$lo < y_test & oob_interval$up > y_test)
      coverage_quantile_interval[k,j] <- mean(quantile_interval$lo < y_test & quantile_interval$up > y_test)
      length_conformal_split[k,j] <- mean(conformal_split$up - conformal_split$lo )
      length_oob_interval[k,j] <- mean(oob_interval$up - oob_interval$lo)
      length_quantile_interval[k,j] <- mean(quantile_interval$up - quantile_interval$lo)
    }
    
    print(c(i,k))
  }
  
  data_result <- list(coverage_oob_interval = coverage_oob_interval,
                      coverage_conformal_split = coverage_conformal_split,
                      coverage_quantile_interval = coverage_quantile_interval,
                      length_conformal_split = length_conformal_split,
                      length_oob_interval = length_oob_interval,
                      length_quantile_interval = length_quantile_interval,
                      cover_mat_conformal_split = cover_mat_conformal_split,
                      cover_mat_oob_interval = cover_mat_oob_interval,
                      cover_mat_quantile_interval = cover_mat_quantile_interval,
                      include_index = include_index,
                      alpha = alpha, n_cv = n_cv, n_rep = n_rep,
                      dataname = datalist[i], x = x, y = y)
  
  saveRDS(data_result, file=paste0("marginprob_coverage/",datalist[i], "_marginprob.rds"))
}






