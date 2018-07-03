setwd("~/Documents/RFInterval/sim/output_combine")
#setwd("~/Documents/RFInterval/sim/output")

output_list <- system("ls", intern = TRUE)

library(ggplot2)
library(reshape2)
library(dplyr)
library(data.table)
library(reshape2)
library(xtable)

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
      #tmp$alpha <- substr(out_info[[1]][6], 1, nchar(out_info[[1]][6])-4)
      tmp$n <- paste0("n=",substr(out_info[[1]][6], 1, nchar(out_info[[1]][6])-4))
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
    #tmp$alpha <- substr(out_info[[1]][6], 1, nchar(out_info[[1]][6])-4)
    tmp$n <- paste0("n=",substr(out_info[[1]][6], 1, nchar(out_info[[1]][6])-4))
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
            to = c("Linear", "Nonlinear", "Nonlinear with interaction"))
out_marg$error_dist <- 
  factor(
  mapvalues(out_marg$error_dist, from = c("heavy-tailed", "homoscedastic", "heteroscedastic", "skewed"), 
            to = c("Heavy-tailed", "Homoscedastic", "Heteroscedastic", "Skewed")),
  levels = c("Homoscedastic", "Heavy-tailed", "Skewed", "Heteroscedastic"))

out_marg$n <- factor(out_marg$n, levels = c("n=200", "n=500", "n=1000", "n=2000", "n=5000"))
#out_marg$n <- factor(out_marg$n, levels = c("n=100", "n=1000", "n=10000"))

out_marg_cov <- melt(out_marg, id.vars = 7:10, measure.vars = 1:3)
out_marg_cov$method <- out_marg_cov$variable
out_marg_cov$coverage <- out_marg_cov$value
out_marg_cov$method <- 
  mapvalues(out_marg_cov$method, from = c("OOB", "Conformal", "Quantile"), 
            to = c("OOB", "SC", "QRF"))

out_marg_cov <- data.table(out_marg_cov)

#### marginal coverage
result <- out_marg_cov[,list(marginal_coverage=mean(coverage)),
                       by=c("predictor_dist","mean_function","error_dist","n","variable")]

result <- dcast(result, predictor_dist+mean_function+error_dist~variable+n, value.var = "marginal_coverage")
xtable(result, digits = 3)


##### conditional coverage 
ggplot(subset(out_marg_cov, predictor_dist=="Correlated predictor")) + 
  geom_boxplot(aes(x = method, y = coverage,fill = n),outlier.size = 0.5) +
  facet_wrap(mean_function~error_dist, ncol = 3)+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage, group=n),
               shape=2, size=1, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_coverage_correlated.pdf",
       width = 8, height = 8)

ggplot(subset(out_marg_cov, predictor_dist=="Uncorrelated predictor")) + 
  geom_boxplot(aes(x = method, y = coverage,fill = n),outlier.size = 0.5) +
  facet_wrap(mean_function~error_dist, ncol = 3,scales="free_y")+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage, group=n), 
               shape=2, size=1, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_coverage_uncorrelated.pdf",
       width = 8, height = 8)


out_marg_len <- melt(out_marg, id.vars = 7:10, measure.vars = 4:6)
out_marg_len$method <- out_marg_len$variable
out_marg_len$length <- out_marg_len$value

out_marg_len$method <- 
  mapvalues(out_marg_len$method, from = c("OOB.len", "Conformal.len", "Quantile.len"), 
            to = c("OOB", "SC", "QRF"))

ggplot(subset(out_marg_len, predictor_dist=="Uncorrelated predictor"&length<30)) + 
  geom_boxplot(aes(x = method, y = length,fill = n), outlier.size = 0.5) +
  # stat_summary(fun.y=mean, geom="point",
  #              aes(x=method, y = length, group=n), 
  #              shape=2, size=1, position=position_dodge(width=0.75))+
  facet_wrap(mean_function~error_dist, scales="free_y", ncol = 3)+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Interval width") + xlab("Method")
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_width_uncorrelated.pdf",
       width = 8, height = 8)

ggplot(subset(out_marg_len, predictor_dist=="Correlated predictor"&length<30)) + 
  geom_boxplot(aes(x = method, y = length,fill = n), outlier.size = 0.5) +
  # stat_summary(fun.y=mean, geom="point",
  #              aes(x=method, y = length, group=n), 
  #              shape=2, size=1, position=position_dodge(width=0.75))+
  facet_wrap(mean_function~error_dist, scales="free_y", ncol = 3)+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Interval width") + xlab("Method")
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_width_correlated.pdf",
       width = 8, height = 8)

out_marg_len$ratio[out_marg_len$method=="OOB"] <- 1
out_marg_len$ratio[out_marg_len$method=="SC"] <- 
  log2(out_marg_len$length[out_marg_len$method=="SC"]/
         out_marg_len$length[out_marg_len$method=="OOB"])
out_marg_len$ratio[out_marg_len$method=="QRF"] <- 
  log2(out_marg_len$length[out_marg_len$method=="QRF"]/
         out_marg_len$length[out_marg_len$method=="OOB"])

ggplot(subset(out_marg_len, predictor_dist=="Correlated predictor"&
                method!="OOB"&ratio<1&ratio>-0.5)) + 
  geom_boxplot(aes(x = method, y = ratio,fill = n), outlier.size = 0.5) +
  # stat_summary(fun.y=mean, geom="point",
  #              aes(x=method, y = ratio, group=n), 
  #              shape=2, size=1, position=position_dodge(width=0.75))+
  facet_wrap(mean_function~error_dist, scales="free_y", ncol = 3)+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab(expression(paste("log"[2], "(Interval width ratio)"))) + xlab(NULL)+
  geom_hline(yintercept = 0, linetype="dashed", alpha = 1)+
  scale_x_discrete(labels=c("SC" = "SC/OOB", "QRF" = "QRF/OOB"))
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_width_ratio_correlated.pdf",
       width = 8, height = 8)

ggplot(subset(out_marg_len, predictor_dist=="Uncorrelated predictor"&method!="OOB"&ratio<1&ratio>-1)) + 
  geom_boxplot(aes(x = method, y = ratio,fill = n), outlier.size = 0.5) +
  # stat_summary(fun.y=mean, geom="point",
  #              aes(x=method, y = ratio, group=n), 
  #              shape=2, size=1, position=position_dodge(width=0.75))+
  facet_wrap(mean_function~error_dist, scales="free_y", ncol = 3)+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab(expression(paste("log"[2], "(Interval width ratio)"))) + xlab(NULL)+
  geom_hline(yintercept = 0, linetype="dashed", alpha = 0.8)+
  scale_x_discrete(labels=c("SC" = "SC/OOB", "QRF" = "QRF/OOB"))
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_width_ratio_uncorrelated.pdf",
       width = 8, height = 8)

#######################
out_cond$x0 <- as.factor(out_cond$x0)
out_cond_cov <- melt(out_cond, id.vars = 7:11, measure.vars = 1:3)
out_cond_cov$method <- out_cond_cov$variable
out_cond_cov$coverage <- out_cond_cov$value
# ggplot(subset(out_cond_cov, x0 %in% c("4"))) +
#   geom_point(aes(x = method, y = coverage,  colour = predictor_dist)) +
#   facet_grid(error_dist~mean_function)+
#   geom_hline(yintercept = 0.9, linetype="dashed", alpha = 0.8)+
#   theme(legend.position = "bottom")

# out_cond_cov <- data.table(out_cond_cov)
# out_cond_cov <- 
#   out_cond_cov[,list(conditional_coverage=mean(coverage)),
#              by=c("x0","predictor_dist","mean_function","error_dist","n","method")]

out_cond_cov$predictor_dist <- 
  mapvalues(out_cond_cov$predictor_dist, from = c("uncorrelated", "correlated"), 
            to = c("Uncorrelated predictor", "Correlated predictor"))
out_cond_cov$mean_function <- 
  mapvalues(out_cond_cov$mean_function, from = c("linear", "nonlinear", "nonlinear-interaction"), 
            to = c("Linear", "Nonlinear", "Nonlinear with interaction"))
out_cond_cov$error_dist <- 
  factor(
    mapvalues(out_cond_cov$error_dist, from = c("heavy-tailed", "homoscedastic", "heteroscedastic"), 
              to = c("Heavy-tailed", "Homoscedastic", "Heteroscedastic")),
    levels = c("Homoscedastic", "Heavy-tailed", "Heteroscedastic"))

out_cond_cov$method <- 
  mapvalues(out_cond_cov$method, from = c("OOB", "Conformal", "Quantile"), 
            to = c("OOB", "SC", "QRF"))

out_cond_cov$n <- factor(out_cond_cov$n, levels = c("n=200", "n=500", "n=1000", "n=2000", "n=5000"))

ggplot(subset(out_cond_cov, x0==1&predictor_dist=="Correlated predictor")) + 
  geom_boxplot(aes(x = method, y = coverage, fill = n), outlier.size = 0.5) +
  facet_wrap(mean_function~error_dist,scales="free_y")+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage, group=n), 
               shape=2, size=1, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_cond_coverage_correlated_x1.pdf",
       width = 8, height = 8)

ggplot(subset(out_cond_cov, x0==2&predictor_dist=="Correlated predictor"&coverage>0.5)) + 
  geom_boxplot(aes(x = method, y = coverage, fill = n), outlier.size = 0.5) +
  facet_wrap(mean_function~error_dist,scales="free_y")+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage, group=n), 
               shape=2, size=1, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_cond_coverage_correlated_x2.pdf",
       width = 8, height = 8)

ggplot(subset(out_cond_cov, x0==1&predictor_dist=="Uncorrelated predictor"&coverage>0.5)) + 
  geom_boxplot(aes(x = method, y = coverage, fill = n), outlier.size = 0.5) +
  facet_wrap(mean_function~error_dist,scales="free_y")+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage, group=n), 
               shape=2, size=1, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_cond_coverage_uncorrelated_x1.pdf",
       width = 8, height = 8)

ggplot(subset(out_cond_cov, x0==2&predictor_dist=="Uncorrelated predictor"&coverage>0.5)) + 
  geom_boxplot(aes(x = method, y = coverage, fill = n), outlier.size = 0.5) +
  facet_wrap(mean_function~error_dist,scales="free_y")+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage, group=n), 
               shape=2, size=1, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
ggsave("~/Documents/Paper/manuscript_v7/figures/sim_cond_coverage_uncorrelated_x2.pdf",
       width = 8, height = 8)

# out_cond <- out_cond %>%
#   #select(x0==1)%>%
#   group_by(predictor_dist, mean_function, error_dist, x0) %>%
#   summarise(OOB=mean(OOB), Conformal=mean(Conformal), Quantile=mean(Quantile))
# 
# out_cond <- as.data.frame(out_cond)
# out_cond <- cbind(out_cond[out_cond$x0 == 1, c(1:3,5:7)],
#                   out_cond[out_cond$x0 == 2, c(5:7)],
#                   out_cond[out_cond$x0 == 3, c(5:7)])
# 
# library(xtable)
# print(xtable(out_cond, digits = 3), include.rownames=FALSE)
# 
