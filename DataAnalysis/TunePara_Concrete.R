#setwd("/Users/haozhe/Documents/RFInterval/DataAnalysis/")
setwd("~/RFInterval/DataAnalysis/")

#library(devtools)
#install_github(repo="ryantibs/conformal", subdir="conformalInference")
library(randomForest)
library(quantregForest)
#library(caret)
library(cvTools)
#library(conformalInference)
for(i in 1:16)
  source(system("ls ~/RFInterval/DataAnalysis/conformalInference/*.R", intern = TRUE)[i])
  #source(system("ls /Users/haozhe/Documents/RFInterval/DataAnalysis/conformalInference/*.R", intern = TRUE)[i])
#my.rf.funs = rf.funs()
#library(randomForestCI)

source("RFOOBInterval.R")
source("RFQuanInterval.R")

alpha <- 0.1
n_rep <- 20
Kfold = 5
ntree = 2000


coverage_conformal_split <-
  coverage_oob_interval <-
  coverage_quantile_interval <-
  length_conformal_split <-
  length_oob_interval <-
  length_quantile_interval <-
  matrix(NA, n_rep, Kfold)

x <- data.matrix(read.table(paste0('UCI/','ConcreteCompressiveStrength','/x.txt'),head=F))
y <- scan(paste0('UCI/','ConcreteCompressiveStrength','/y.txt'),0)

for(nodesize in c(1,5)){
  for(mtry in c(2,3,6)){
    for(k in 1:n_rep){
      partitions <- cvFolds(length(y), K=Kfold)
      for(j in 1:Kfold){
        x_train = x[partitions$subsets[partitions$which != j],]
        y_train = y[partitions$subsets[partitions$which != j]]
        x_test = x[partitions$subsets[partitions$which == j],]
        y_test = y[partitions$subsets[partitions$which == j]]
        
        my.rf.funs = rf.funs(ntree = ntree, mtry = mtry, nodesize = nodesize)
        
        conformal_split <-  conformal.pred.split(x_train, y_train, x_test,
                                                 alpha = alpha, verb= FALSE,
                                                 train.fun=my.rf.funs$train,
                                                 predict.fun=my.rf.funs$predict)
        
        oob_interval <- RFOOBInterval(x_train, y_train, x_test, alpha = alpha, symmetry = TRUE, 
                                      ntree = ntree, mtry=mtry, nodesize=nodesize)
        quantile_interval <- RFQuanInterval(x_train, y_train, x_test, alpha = alpha,
                                            ntree = ntree, mtry=mtry, nodesize=nodesize)
        
        coverage_conformal_split[k,j] <- mean(conformal_split$lo < y_test & conformal_split$up > y_test)
        coverage_oob_interval[k,j] <- mean(oob_interval$lo < y_test & oob_interval$up > y_test)
        coverage_quantile_interval[k,j] <- mean(quantile_interval$lo < y_test & quantile_interval$up > y_test)
        length_conformal_split[k,j] <- mean(conformal_split$up - conformal_split$lo )
        length_oob_interval[k,j] <- mean(oob_interval$up - oob_interval$lo)
        length_quantile_interval[k,j] <- mean(quantile_interval$up - quantile_interval$lo)
        
        print(paste0("Concrete", "(", i, ")"))
        print(paste0("OOB: ", coverage_oob_interval[k,j], ";",
                     "Conformal: ", coverage_conformal_split[k,j], ";",
                     "Quantile: ", coverage_quantile_interval[k,j], ";"))
      }
      print(k)
    }
    data_result <- list(coverage_oob_interval = coverage_oob_interval,
                        coverage_conformal_split = coverage_conformal_split,
                        coverage_quantile_interval = coverage_quantile_interval,
                        length_oob_interval = length_oob_interval,
                        length_conformal_split = length_conformal_split,
                        length_quantile_interval = length_quantile_interval)
    
    saveRDS(data_result, file=paste0("TunePara_Concrete_output/",'nodesize_',nodesize,'_mtry_',mtry, "_.rds"))
    
  }
}

