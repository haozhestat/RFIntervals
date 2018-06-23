## Test out conformal intervals and our proposed prediction intervals
## across a variety of real datasets

setwd("/Users/haozhe/Documents/RFInterval")
library(randomForest)
library(quantregForest)
#library(devtools)
#install_github(repo="ryantibs/conformal", subdir="conformalInference")
#install_github("swager/randomForestCI")
library(conformalInference)
library(randomForestCI)

source("my.pred.rf.fun.R")
source("my.quantile.rf.fun.R")
source("ExtractWeights.R")
MSE.test <- rep(0,length(datalist))
my.rf.funs = rf.funs()
setwd("/Users/haozhe/Documents/RFInterval/nipsdata")
datalist <- system('ls data',intern=T)
current.dir <- getwd()
dataset.result <- data.frame(name = datalist, 
                             num.predictor = NA, 
                             samp.size.train = NA, 
                             samp.size.test = NA,
                             Cov.PredInt = NA, 
                             Cov.Quant = NA, 
                             Cov.InfJack = NA, 
                             Cov.ConfSplit = NA, 
                             #Cov.ConfJack = NA,
                             Len.PredInt = NA, 
                             Len.Quant = NA, 
                             Len.InfJack = NA, 
                             Len.ConfSplit = NA
                             #Len.ConfJack = NA
                             )

for(i in 1:length(datalist)){
  print(i)
  setwd(paste(current.dir,'/data/',datalist[i],sep=''))
  x <- read.table('x.txt',head=F)
  y <- scan('y.txt',0)
  #orig.y <- scan('origy.txt',0) # probably not needed
  partition <- read.table('partitions.txt',head=F) 
  dataset.result$num.predictor[i] <- ncol(x)
  dataset.result$samp.size.train[i] <- sum(partition[,1]!=0)
  dataset.result$samp.size.test[i] <- sum(partition[,1]==0) 
  
  #### Our proposed Prediction Interval
  result.PredInt <- my.pred.rf.fun(x[partition[,1]!=0,], 
                                   y[partition[,1]!=0],
                                   x[partition[,1]==0,])
  dataset.result$Cov.PredInt[i] <- 
    mean(result.PredInt$lo <= y[partition[,1]==0] & y[partition[,1]==0] <= result.PredInt$up)
  dataset.result$Len.PredInt[i] <- mean(result.PredInt$up - result.PredInt$lo)
  
  #### Quantile Random Forest
  result.Quant <- my.quantile.rf.fun(x[partition[,1]!=0,], 
                                     y[partition[,1]!=0],
                                     x[partition[,1]==0,])
  dataset.result$Cov.Quant[i] <- 
    mean(result.Quant$lo <= y[partition[,1]==0] & y[partition[,1]==0] <= result.Quant$up)
  dataset.result$Len.Quant[i] <- mean(result.Quant$up - result.Quant$lo)
  
  #### Infite Jacknife
  rf = randomForest(x[partition[,1]!=0,], y[partition[,1]!=0], keep.inbag = TRUE)
  ij = randomForestInfJack(rf, x[partition[,1]==0,], calibrate = TRUE)
  dataset.result$Cov.InfJack[i] <- 
    mean(ij$y.hat - qnorm(0.95)*sqrt(ij$var.hat) <= y[partition[,1]==0] & 
           y[partition[,1]==0] <= ij$y.hat + qnorm(0.95)*sqrt(ij$var.hat))
  dataset.result$Len.InfJack[i] <- mean(2*qnorm(0.95)*sqrt(ij$var.hat))
  
  #### Conformal Split
  result.ConfSplit <- conformal.pred.split(x[partition[,1]!=0,], 
                                           y[partition[,1]!=0],
                                           as.matrix(x[partition[,1]==0,]),
                                           alpha=0.1,
                                           verb="\t\t",
                                           train.fun=my.rf.funs$train,
                                           predict.fun=my.rf.funs$predict)
  dataset.result$Cov.ConfSplit[i] <- 
    mean(result.ConfSplit$lo <= y[partition[,1]==0] & y[partition[,1]==0] <= result.ConfSplit$up)
  dataset.result$Len.ConfSplit[i] <- mean(result.ConfSplit$up - result.ConfSplit$lo)
  
  #### Conformal Jacknife
  # result.ConfJack <- conformal.pred.jack(x = x[partition[,1]!=0,], 
  #                                        y = y[partition[,1]!=0],
  #                                        x0 = as.matrix(x[partition[,1]==0,]),
  #                                        alpha=0.1,
  #                                        verb="\t\t",
  #                                        train.fun=my.rf.funs$train,
  #                                        predict.fun=my.rf.funs$predict)
  # dataset.result$Cov.ConfJack[i] <- 
  #   mean(result.ConfJack$lo <= y[partition[,1]==0] & y[partition[,1]==0] <= result.ConfJack$up)
  # dataset.result$Len.ConfJack[i] <- mean(result.ConfJack$up - result.ConfJack$lo)

  print(dataset.result[i,])
}

library(xtable)
rownames(dataset.result) <- NULL
tmp <- dataset.result[order(dataset.result$samp.size.test, decreasing = TRUE),]
print(xtable(tmp, digits = 4))
saveRDS(dataset.result,file="~/Documents/RFInterval/dataset.result.RDS")
