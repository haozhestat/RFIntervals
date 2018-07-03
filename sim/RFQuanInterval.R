RFQuanInterval <- function(x, y, x0, alpha = 0.10, ntree = 500,
                          mtry = if (!is.null(y) && !is.factor(y))
                            max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
                          nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1){
  
  x <- as.matrix(x)
  x0 <- as.matrix(x0)
  colnames(x) <- 1:ncol(x)
  rownames(x) <- 1:nrow(x)
  colnames(x0) <- 1:ncol(x0)
  rownames(x0) <- 1:nrow(x0)
  
  ntest = nrow(x0)

  quanrf = quantregForest(x, y, mtry = mtry, nodesize = nodesize,ntree = ntree)
  
  return(list(lo = matrix(predict(quanrf, x0, alpha/2), ntest, 1), 
              up = matrix(predict(quanrf, x0, 1-alpha/2), ntest, 1)))
}
