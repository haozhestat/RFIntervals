setwd("~/Documents/RFInterval/DataAnalysis/TunePara_Concrete_output/")

output_list <- system("ls", intern = TRUE)

library(ggplot2)
library(reshape2)
library(plyr)

out_marg <- NULL

for(k in 1:length(output_list)){
  output <- readRDS(output_list[k])
  out_info <- strsplit(output_list[k], split = "_")
  
  tmp <- data.frame(OOB = c(output[[1]]),
                    Conformal = c(output[[2]]),
                    Quantile = c(output[[3]]),
                    OOB.len = c(output[[4]]),
                    Conformal.len = c(output[[5]]),
                    Quantile.len = c(output[[6]]),
                    nodesize = paste0("nodesize=", out_info[[1]][2]),
                    mtry = paste0("mtry=", out_info[[1]][4]))
  out_marg <- rbind(out_marg, tmp)
}

out_marg_cov <- melt(out_marg[,c(1:3,7,8)], measure.vars = 1:3)
out_marg_cov$method <- out_marg_cov$variable
out_marg_cov$coverage <- out_marg_cov$value
out_marg_cov$method <- 
  mapvalues(out_marg_cov$method, from = c("OOB", "Conformal", "Quantile"), 
            to = c("OOB", "CONF", "QRF"))

plot_coverage <- 
  ggplot(out_marg_cov) + 
  geom_boxplot(aes(x = method, y = coverage), outlier.size = 0.5) +
  facet_wrap(nodesize~mtry)+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage),
               shape=2, size=2, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  #theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
plot_coverage
ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_TunePara_Coverage.pdf",
       width = 6, height = 3.5)

out_marg_len <- melt(out_marg[,4:8], measure.vars = 1:3)
out_marg_len$method <- out_marg_len$variable
out_marg_len$Width <- out_marg_len$value
out_marg_len$method <- 
  mapvalues(out_marg_len$method, from = c("OOB.len", "Conformal.len", "Quantile.len"), 
            to = c("OOB", "CONF", "QRF"))

#out_marg_len <- subset(out_marg_len, length<30)
plot_width <- 
  ggplot(out_marg_len) + 
  geom_boxplot(aes(x = method, y = Width), outlier.size = 0.5) +
  facet_wrap(nodesize~mtry)+
  #facet_wrap(~mean_function+error_dist, scales="free_y")+
  #geom_hline(yintercept = 1, linetype="dashed")+
  # theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Interval width") + xlab("Method")
plot_width
ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_TunePara_Width.pdf",
       width = 6, height = 3.5)




