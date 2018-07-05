setwd("~/RFInterval/DataAnalysis/output/")

output_list <- system("ls", intern = TRUE)
DataNameChange <- read.csv("../DataNameChange.csv", header = TRUE)

library(ggplot2)
library(reshape2)
library(plyr)
library(latex2exp)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

out_marg <- NULL

for(k in 1:length(output_list)){
  output <- readRDS(output_list[k])
  out_info <- strsplit(output_list[k], split = "_")

  if(!(out_info[[1]][1] %in%c("ForestFires","Labor","SkillCraft1","CommunityCrimeUnnorm"))){
    out_info[[1]][1] <- as.character(DataNameChange$New_Name[DataNameChange$Old_Name==out_info[[1]][1]])
    tmp <- data.frame(OOB = mean(output[[1]], na.rm = TRUE),
                      Conformal = mean(output[[2]], na.rm = TRUE),
                      Quantile = mean(output[[3]], na.rm = TRUE),
                      Conformal.len_OOB.len = mean(log2(c(output[[5]])/c(output[[4]])), na.rm = TRUE),
                      Quantile.len_OOB.len = mean(log2(c(output[[6]])/c(output[[4]])), na.rm = TRUE),
                      data = out_info[[1]][1])
    out_marg <- rbind(out_marg, tmp)
  }
}

out_marg$ave <- rowMeans(out_marg[,1:3])
out_marg$data <- factor(out_marg$data, levels = out_marg$data[order(out_marg$ave)])
out_marg_cov <- melt(out_marg[,c(1:3,6)], measure.vars = 1:3)
out_marg_cov$method <- out_marg_cov$variable
out_marg_cov$coverage <- out_marg_cov$value
out_marg_cov$method <- 
  mapvalues(out_marg_cov$method, from = c("OOB", "Conformal", "Quantile"), 
            to = c("OOB", "CONF", "QRF"))

out_marg_len <- melt(out_marg[,4:6], measure.vars = 1:2)
out_marg_len$method <- out_marg_len$variable
out_marg_len$Width_Ratio <- out_marg_len$value
out_marg_len$method <- 
  mapvalues(out_marg_len$method, from = c("Conformal.len_OOB.len", "Quantile.len_OOB.len"), 
            to = c("CONF_width/OOB_width", "QRF_width/OOB_width"))
labels <- expression(paste("log"[2], "(CONF_width/OOB_width)"),
                     paste("log"[2], "(QRF_width/OOB_width)"))

plot_coverage_1 <- 
  ggplot(subset(out_marg_cov, data %in% unique(out_marg_cov$data)[1:20])) + 
  geom_boxplot(aes(x = method, y = coverage), outlier.size = 0.5) +
  facet_wrap(~data, nrow=5,ncol=4,scales="free_y")+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage),
               shape=2, size=2, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  xlab("Method") + ylab("Interval coverage rate")
plot_coverage_1
ggsave("~/Documents/Paper/manuscript_v7/figures/DataAnalysis_Cond_Coverage_by_data_1.pdf",
       width = 8, height = 10)

plot_coverage_2 <- 
  ggplot(subset(out_marg_cov, data %in% unique(out_marg_cov$data)[21:40])) + 
  geom_boxplot(aes(x = method, y = coverage), outlier.size = 0.5) +
  facet_wrap(~data, nrow=5, ncol=4,scales="free_y")+
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage),
               shape=2, size=2, position=position_dodge(width=0.75))+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  xlab("Method") + ylab("Interval coverage rate")
plot_coverage_2
ggsave("~/Documents/Paper/manuscript_v7/figures/DataAnalysis_Cond_Coverage_by_data_2.pdf",
       width = 8, height = 10)

plot_coverage_3 <- 
  ggplot(subset(out_marg_cov, data %in% unique(out_marg_cov$data)[41:60])) + 
  geom_boxplot(aes(x = method, y = coverage), outlier.size = 0.5) +
  stat_summary(fun.y=mean, geom="point",
               aes(x=method, y = coverage),
               shape=2, size=2, position=position_dodge(width=0.75))+
  facet_wrap(~data, ncol=4,scales="free_y")+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  xlab("Method") + ylab("Interval coverage rate")
plot_coverage_3
ggsave("~/Documents/Paper/manuscript_v7/figures/DataAnalysis_Cond_Coverage_by_data_3.pdf",
       width = 8, height = 10)

plot_width <- 
  ggplot(out_marg_len) + 
  geom_point(aes(x = data, y = Width_Ratio, color=method, shape=method),
             data=out_marg_len) +
  geom_hline(yintercept = 0, linetype="dashed")+
  scale_color_manual(values = gg_color_hue(2),
                     labels=labels) +
  scale_shape_manual(values = c(15,16),
                     labels=labels)+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab(expression(paste("log"[2], "(Interval width ratio)"))) + xlab("Dataset")+ ylim(c(-1,2))
plot_width
ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_Width_aggregate.pdf",
       width = 8, height = 4.5)



out_cond <- NULL
for(k in 1:length(output_list)){
  output <- readRDS(output_list[k])
  out_info <- strsplit(output_list[k], split = "_")
  
  if(!(out_info[[1]][1] %in%c("ForestFires","Labor","SkillCraft1","CommunityCrimeUnnorm"))){
    out_info[[1]][1] <- as.character(DataNameChange$New_Name[DataNameChange$Old_Name==out_info[[1]][1]])
    tmp <- data.frame(OOB = c(output[[1]]),
                    Conformal = c(output[[2]]),
                    Quantile = c(output[[3]]),
                    Conformal.len_OOB.len = log2(c(output[[5]])/c(output[[4]])),
                    Quantile.len_OOB.len = log2(c(output[[6]])/c(output[[4]])),
                    data = out_info[[1]][1])
  out_cond <- rbind(out_cond, tmp)
  }
}

out_cond$data <- factor(out_cond$data, levels = out_marg$data[order(out_marg$ave)])
out_cond_cov <- melt(out_cond[,c(1:3,6)], measure.vars = 1:3)
out_cond_cov$method <- out_cond_cov$variable
out_cond_cov$coverage <- out_cond_cov$value
out_cond_cov$method <- 
  mapvalues(out_cond_cov$method, from = c("OOB", "Conformal", "Quantile"), 
            to = c("OOB", "CONF", "QRF"))

plot_coverage <-
  ggplot(out_cond_cov) + 
  geom_boxplot(aes(x = data, y = coverage), outlier.size = 0.5) +
  facet_wrap(~method, nrow=3,scales="free_y")+
  geom_hline(yintercept = 0.9, linetype="dashed")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab("Dataset") + ylab("Interval coverage rate")
plot_coverage
ggsave("~/Documents/Paper/manuscript_v5/figures/DataAnalysis_Cond_Coverage_by_method.pdf",
       width = 8, height = 8)
