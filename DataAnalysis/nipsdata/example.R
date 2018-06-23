setwd("/Users/haozhe/Documents/RFInterval/nipsdata")
datalist <- system('ls data',intern=T)
current.dir <- getwd()
MSE.test <- rep(0,length(datalist))
for (i in 1:length(datalist)){
   print(i)
   setwd(paste(current.dir,'/data/',datalist[i],sep=''))
   x <- read.table('x.txt',head=F)
   y <- scan('y.txt',0)
   orig.y <- scan('origy.txt',0) # probably not needed
   partition <- read.table('partitions.txt',head=F) 
   lm1 <- lm(y~.,data=data.frame(x=x,y=y)[partition[,1]!=0,])
   # NOTE that the above is not correct for all problems.  I am assuming
   # that all predictors are numeric.  In some cases, they will be
   # categorical and dummy variables will be needed to be constructed.

   # ALSO NOTE that I'm only looking at one partition (partition[,1]) and
   # not doing any cross-validation using folds 1-5.

   yhat <- predict(lm1,newdata=data.frame(x=x,y=y)[partition[,1]==0,])
   MSE.test[i] <- mean((yhat-y[partition[,1]==0])^2)
}
setwd(current.dir)
print(mean(sqrt(MSE.test))) # should be 0.6129854
