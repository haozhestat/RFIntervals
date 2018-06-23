setwd("~/Documents/RFInterval/sim/output")

output_list <- system("ls", intern = TRUE)

library(ggplot2)
library(reshape2)

out_marg <- NULL
out_cond <- NULL

for(k in 1:length(output_list)){
  output <- readRDS(output_list[k])
  out_info <- strsplit(output_list[k], split = "_")
  
  if(out_info[[1]][2] == "cond"){
    for(j in 1:ncol(output[[1]])){
      tmp <- data.frame(OOB = output[[1]][,j],
                        Conformal = output[[2]][,j],
                        Quantile = output[[3]][,j],
                        OOB.len = output[[4]][,j],
                        Conformal.len = output[[5]][,j],
                        Quantile.len = output[[6]][,j])
      tmp$x0 <- j
      tmp$predictor_dist <- out_info[[1]][3]
      tmp$mean_function <- out_info[[1]][4]
      tmp$error_dist <- out_info[[1]][5]
      tmp$alpha <- substr(out_info[[1]][6], 1, nchar(out_info[[1]][6])-4)
      out_cond <- rbind(out_cond, tmp)
    }
  }
  else{
    tmp <- data.frame(OOB = output[[1]],
                      Conformal = output[[2]],
                      Quantile = output[[3]],
                      OOB.len = output[[4]],
                      Conformal.len = output[[5]],
                      Quantile.len = output[[6]])
    tmp$predictor_dist <- out_info[[1]][3]
    tmp$mean_function <- out_info[[1]][4]
    tmp$error_dist <- out_info[[1]][5]
    tmp$alpha <- substr(out_info[[1]][6], 1, nchar(out_info[[1]][6])-4)
    out_marg <- rbind(out_marg, tmp)
  }
}

# library(latex2exp)
# facet_label <- c("linear" = TeX('$m_{1}(\\mathbf{X})$'),
#                     "nonlinear" = TeX('$m_{2}(\\mathbf{X})$'),
#                     "nonlinear-interaction" = TeX('$m_{1}(\\mathbf{X})$'))
library(plyr)
out_marg$predictor_dist <- 
  mapvalues(out_marg$predictor_dist, from = c("uncorrelated", "correlated"), 
          to = c("Uncorrelated predictor", "Correlated predictor"))
out_marg$mean_function <- 
  mapvalues(out_marg$mean_function, from = c("linear", "nonlinear", "nonlinear-interaction"), 
            to = c("Linear mean function", "Nonlinear  mean function", "Nonlinear  mean function with interaction"))
out_marg$error_dist <- 
  factor(
  mapvalues(out_marg$error_dist, from = c("heavy-tailed", "homoscedastic", "heteroscedastic"), 
            to = c("Heavy-tailed error", "Homoscedastic error", "Heteroscedastic error")),
  levels = c("Homoscedastic error", "Heteroscedastic error", "Heavy-tailed error"))

out_marg_cov <- melt(out_marg, id.vars = 7:10, measure.vars = 1:3)
out_marg_cov$method <- out_marg_cov$variable
out_marg_cov$coverage <- out_marg_cov$value
out_marg_cov$method <- 
  mapvalues(out_marg_cov$method, from = c("OOB", "Conformal", "Quantile"), 
            to = c("OOB", "CONF", "QUAN"))
ggplot(out_marg_cov) + geom_boxplot(aes(x = method, y = coverage,  
                                      fill = predictor_dist), 
                                    outlier.size = 0.5) +
  facet_wrap(mean_function~error_dist)+
  geom_hline(yintercept = 0.9, linetype="dashed", alpha = 0.8)+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")

out_marg_len <- melt(out_marg, id.vars = 7:10, measure.vars = 4:6)
out_marg_len$method <- out_marg_len$variable
out_marg_len$length <- out_marg_len$value

out_marg_len$method <- 
  mapvalues(out_marg_len$method, from = c("OOB.len", "Conformal.len", "Quantile.len"), 
            to = c("OOB", "CONF", "QUAN"))
#out_marg_len <- subset(out_marg_len, length<30)
ggplot(out_marg_len) + geom_boxplot(aes(x = method, y = length, 
                                        fill = predictor_dist), 
                                    outlier.size = 0.5) +
  facet_wrap(~mean_function+error_dist, scales="free_y")+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Interval width") + xlab("Method")

# out_cond$x0 <- as.factor(out_cond$x0)
# out_cond_cov <- melt(out_cond, id.vars = 7:11, measure.vars = 1:3)
# out_cond_cov$method <- out_cond_cov$variable
# out_cond_cov$coverage <- out_cond_cov$value
# ggplot(subset(out_cond_cov, x0 %in% c("4"))) +
#   geom_point(aes(x = method, y = coverage,  colour = predictor_dist)) +
#   facet_grid(error_dist~mean_function)+
#   geom_hline(yintercept = 0.9, linetype="dashed", alpha = 0.8)+
#   theme(legend.position = "bottom")


out_cond$predictor_dist <- 
  mapvalues(out_cond$predictor_dist, from = c("uncorrelated", "correlated"), 
            to = c("Uncorrelated predictor", "Correlated predictor"))
out_cond$mean_function <- 
  mapvalues(out_cond$mean_function, from = c("linear", "nonlinear", "nonlinear-interaction"), 
            to = c("Linear m(X)", "Nonlinear m(X)", "Nonlinear m(X) with interaction"))
out_cond$error_dist <- 
  factor(
    mapvalues(out_cond$error_dist, from = c("heavy-tailed", "homoscedastic", "heteroscedastic"), 
              to = c("Heavy-tailed error", "Homoscedastic error", "Heteroscedastic error")),
    levels = c("Homoscedastic error", "Heavy-tailed error", "Heteroscedastic error"))

out_cond_cov <- melt(out_cond, id.vars = 7:11, measure.vars = 1:3)
out_cond_cov$method <- out_cond_cov$variable
out_cond_cov$coverage <- out_cond_cov$value
out_cond_cov$method <- 
  mapvalues(out_cond_cov$method, from = c("OOB", "Conformal", "Quantile"), 
            to = c("OOB", "CONF", "QUAN"))
ggplot(subset(out_cond_cov, x0 == 2)) + geom_boxplot(aes(x = method, y = coverage, 
                                                         color = predictor_dist)) +
  facet_wrap(mean_function~error_dist)+
  geom_hline(yintercept = 0.9, linetype="dashed", alpha = 0.8)+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")



out_cond <- cbind(out_cond[out_cond$x0 == 1, c(8,9,10,1:3)],
                  out_cond[out_cond$x0 == 2, c(1:3)],
                  out_cond[out_cond$x0 == 3, c(1:3)])
rownames(out_cond) <- NULL
library(xtable)
print(xtable(out_cond, digits = 3), include.rownames=FALSE)

