setwd("~/Documents/RFInterval/DataAnalysis/output/")

output_list <- system("ls", intern = TRUE)
DataNameChange <- read.csv("../DataNameChange.csv", header = TRUE)

library(ggplot2)
library(reshape2)
library(plyr)
#library(plotly)
#library(cowplot)

out_marg <- NULL

for(k in 1:length(output_list)){
  output <- readRDS(output_list[k])
  out_info <- strsplit(output_list[k], split = "_")
  
  if(!(out_info[[1]][1] %in%c("ForestFires","Labor","SkillCraft1","CommunityCrimeUnnorm"))){
    out_info[[1]][1] <- as.character(DataNameChange$New_Name[DataNameChange$Old_Name==out_info[[1]][1]])
    tmp <- data.frame(OOB = c(output[[1]]),
                      Conformal = c(output[[2]]),
                      Quantile = c(output[[3]]),
                      #OOB.len = c(output[[4]]),
                      Conformal.len_OOB.len = c(output[[5]])/c(output[[4]]),
                      Quantile.len_OOB.len = c(output[[6]])/c(output[[4]]),
                      data = out_info[[1]][1])
    out_marg <- rbind(out_marg, tmp)
  }
}

# for(k in 1:length(output_list)){
#   output <- readRDS(output_list[k])
#   out_info <- strsplit(output_list[k], split = "_")
#   
#   tmp <- data.frame(OOB = mean(output[[1]], na.rm = TRUE),
#                     Conformal = mean(output[[2]], na.rm = TRUE),
#                     Quantile = mean(output[[3]], na.rm = TRUE),
#                     #OOB.len = c(output[[4]]),
#                     Conformal.len_OOB.len = mean(c(output[[5]])/c(output[[4]]), na.rm = TRUE),
#                     Quantile.len_OOB.len = mean(c(output[[6]])/c(output[[4]]), na.rm = TRUE),
#                     data = out_info[[1]][1])
#   out_marg <- rbind(out_marg, tmp)
# }

out_marg_cov <- melt(out_marg[,c(1:3,6)], measure.vars = 1:3)
out_marg_cov$method <- out_marg_cov$variable
out_marg_cov$coverage <- out_marg_cov$value
out_marg_cov$method <- 
  mapvalues(out_marg_cov$method, from = c("OOB", "Conformal", "Quantile"), 
            to = c("OOB", "CONF", "QRF"))


# plot_coverage <-
#   ggplot(out_marg_cov) + 
#   geom_boxplot(aes(x = method, y = coverage), outlier.size = 0.5) +
#   geom_hline(yintercept = 0.9, linetype="dashed")+
#   theme(legend.position = "None", legend.title = element_blank()) +
#   xlab("Method") + ylab("Interval coverage rate")
# plot_coverage
# ggsave("~/Documents/Paper/manuscript_v4/figures/DataAnalysis_Cond_Coverage_by_aggreate.pdf",
#        width = 4, height = 4)

# plot_coverage <-
#   ggplot(out_marg_cov) + 
#   geom_boxplot(aes(x = as.numeric(as.factor(data)), y = coverage, colour=data), outlier.size = 0.5) +
#   facet_wrap(~method, nrow=3,scales="free_y")+
#   #stat_summary(fun.y=mean, geom="point",
#   #             aes(x=method, y = coverage), 
#   #             shape=2, size=1, position=position_dodge(width=0.75))+
#   geom_hline(yintercept = 0.9, linetype="dashed")+
#   theme(legend.position = "None", legend.title = element_blank()) +
#   scale_x_continuous(breaks = seq(1, 54, by = 1))+
#   xlab("Dataset No.") + ylab("Interval coverage rate")
# plot_coverage
# ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_Cond_Coverage_by_method.pdf",
#        width = 10, height = 7)

plot_coverage_1 <- 
  ggplot(subset(out_marg_cov, data %in% unique(out_marg_cov$data)[1:20])) + 
  geom_boxplot(aes(x = method, y = coverage), outlier.size = 0.5) +
  facet_wrap(~data, nrow=5,ncol=4,scales="free_y")+
  stat_summary(fun.y=mean, geom="point",
              aes(x=method, y = coverage),
              shape=2, size=2, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  #theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
plot_coverage_1
ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_Cond_Coverage_by_data_1.pdf",
       width = 8, height = 10)

plot_coverage_2 <- 
  ggplot(subset(out_marg_cov, data %in% unique(out_marg_cov$data)[21:40])) + 
  geom_boxplot(aes(x = method, y = coverage), outlier.size = 0.5) +
  facet_wrap(~data, nrow=5, ncol=4,scales="free_y")+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage),
               shape=2, size=2, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  #theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
plot_coverage_2
ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_Cond_Coverage_by_data_2.pdf",
       width = 8, height = 10)

plot_coverage_3 <- 
  ggplot(subset(out_marg_cov, data %in% unique(out_marg_cov$data)[41:60])) + 
  geom_boxplot(aes(x = method, y = coverage), outlier.size = 0.5) +
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage),
               shape=2, size=2, position=position_dodge(width=0.75))+
  facet_wrap(~data, ncol=4,scales="free_y")+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  #theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Method") + ylab("Interval coverage rate")
plot_coverage_3
ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_Cond_Coverage_by_data_3.pdf",
       width = 8, height = 10)

# plot_coverage_4 <- 
#   ggplot(subset(out_marg_cov, data %in% unique(out_marg_cov$data)[49:64])) + 
#   geom_boxplot(aes(x = method, y = coverage), outlier.size = 0.5) +
#   facet_wrap(~data, ncol=4,scales="free_y")+
#   geom_hline(yintercept = 0.9, linetype="dashed")+
#   #theme(legend.position = "bottom", legend.title = element_blank()) +
#   xlab("Method") + ylab("Interval coverage rate")
# plot_coverage_4
# ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_Cond_Coverage_by_data_4.pdf",
#        width = 8, height = 8)

######################
out_marg_len <- melt(out_marg[,4:6], measure.vars = 1:2)
out_marg_len$method <- out_marg_len$variable
out_marg_len$Width_Ratio <- out_marg_len$value

out_marg_len$method <- 
  mapvalues(out_marg_len$method, from = c("Conformal.len_OOB.len", "Quantile.len_OOB.len"), 
            to = c("CONF_width/OOB_width", "QRF_width/OOB_width"))

#out_marg_len <- subset(out_marg_len, length<30)
plot_width <- 
  ggplot(out_marg_len) + 
  geom_boxplot(aes(x = method, y = Width_Ratio), outlier.size = 0.5) +
  #facet_wrap(~mean_function+error_dist, scales="free_y")+
  geom_hline(yintercept = 1, linetype="dashed")+
 # theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("Interval width ratio") + xlab("")
plot_width
#ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_Width_aggregate.pdf",
#       width = 4, height = 4)


# library(latex2exp)
# facet_label <- c("linear" = TeX('$m_{1}(\\mathbf{X})$'),
#                     "nonlinear" = TeX('$m_{2}(\\mathbf{X})$'),
#                     "nonlinear-interaction" = TeX('$m_{1}(\\mathbf{X})$'))
# library(plyr)
# out_marg$predictor_dist <- 
#   mapvalues(out_marg$predictor_dist, from = c("uncorrelated", "correlated"), 
#             to = c("Uncorrelated predictor", "Correlated predictor"))
# out_marg$mean_function <- 
#   mapvalues(out_marg$mean_function, from = c("linear", "nonlinear", "nonlinear-interaction"), 
#             to = c("Linear mean function", "Nonlinear  mean function", "Nonlinear  mean function with interaction"))
# out_marg$error_dist <- 
#   factor(
#     mapvalues(out_marg$error_dist, from = c("heavy-tailed", "homoscedastic", "heteroscedastic"), 
#               to = c("Heavy-tailed error", "Homoscedastic error", "Heteroscedastic error")),
#     levels = c("Homoscedastic error", "Heteroscedastic error", "Heavy-tailed error"))
# 
# 
# 
# out_marg_len <- melt(out_marg[,1:3], measure.vars = 1:3)
# out_marg_len$method <- out_marg_len$variable
# out_marg_len$length <- out_marg_len$value
# 
# out_marg_len$method <- 
#   mapvalues(out_marg_len$method, from = c("OOB.len", "Conformal.len", "Quantile.len"), 
#             to = c("OOB", "CONF", "QUAN"))
# #out_marg_len <- subset(out_marg_len, length<30)
# ggplot(out_marg_len) + geom_boxplot(aes(x = method, y = length, 
#                                         fill = predictor_dist), 
#                                     outlier.size = 0.5) +
#   facet_wrap(~mean_function+error_dist, scales="free_y")+
#   theme(legend.position = "bottom", legend.title = element_blank()) +
#   ylab("Interval width") + xlab("Method")
# 
# # out_cond$x0 <- as.factor(out_cond$x0)
# # out_cond_cov <- melt(out_cond, id.vars = 7:11, measure.vars = 1:3)
# # out_cond_cov$method <- out_cond_cov$variable
# # out_cond_cov$coverage <- out_cond_cov$value
# # ggplot(subset(out_cond_cov, x0 %in% c("4"))) +
# #   geom_point(aes(x = method, y = coverage,  colour = predictor_dist)) +
# #   facet_grid(error_dist~mean_function)+
# #   geom_hline(yintercept = 0.9, linetype="dashed", alpha = 0.8)+
# #   theme(legend.position = "bottom")
# 
# 
# out_cond$predictor_dist <- 
#   mapvalues(out_cond$predictor_dist, from = c("uncorrelated", "correlated"), 
#             to = c("Uncorrelated predictor", "Correlated predictor"))
# out_cond$mean_function <- 
#   mapvalues(out_cond$mean_function, from = c("linear", "nonlinear", "nonlinear-interaction"), 
#             to = c("Linear m(X)", "Nonlinear m(X)", "Nonlinear m(X) with interaction"))
# out_cond$error_dist <- 
#   factor(
#     mapvalues(out_cond$error_dist, from = c("heavy-tailed", "homoscedastic", "heteroscedastic"), 
#               to = c("Heavy-tailed error", "Homoscedastic error", "Heteroscedastic error")),
#     levels = c("Homoscedastic error", "Heavy-tailed error", "Heteroscedastic error"))
# 
# out_cond_cov <- melt(out_cond, id.vars = 7:11, measure.vars = 1:3)
# out_cond_cov$method <- out_cond_cov$variable
# out_cond_cov$coverage <- out_cond_cov$value
# out_cond_cov$method <- 
#   mapvalues(out_cond_cov$method, from = c("OOB", "Conformal", "Quantile"), 
#             to = c("OOB", "CONF", "QUAN"))
# ggplot(subset(out_cond_cov, x0 == 2)) + geom_boxplot(aes(x = method, y = coverage, 
#                                                          color = predictor_dist)) +
#   facet_wrap(mean_function~error_dist)+
#   geom_hline(yintercept = 0.9, linetype="dashed", alpha = 0.8)+
#   theme(legend.position = "bottom", legend.title = element_blank()) +
#   xlab("Method") + ylab("Interval coverage rate")
# 
# 
# 
# out_cond <- cbind(out_cond[out_cond$x0 == 1, c(8,9,10,1:3)],
#                   out_cond[out_cond$x0 == 2, c(1:3)],
#                   out_cond[out_cond$x0 == 3, c(1:3)])
# rownames(out_cond) <- NULL
# library(xtable)
# print(xtable(out_cond, digits = 3), include.rownames=FALSE)
# 
