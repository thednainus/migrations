#plot the credible interval for migration rates
library(ggplot2)

#read file with the confidence interval
ace_CIs <- readRDS("Analysis_SD/ape_ace/summary/confidence_interval/ace_confidence_intervals.RDS")
ace_CIs$mig_rate <- as.factor(ace_CIs$mig_rate)

ace_CIs$tips <- factor(ace_CIs$tips, levels = c("region1000global100", "region1000global500", "region1000global1000"))




quartz()

# src to region: 1,000bp ----
ggplot(ace_CIs, aes(x = mig_rate, y = median_1000bp_1, colour = tips)) +
  geom_point(size = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymax = upper_1000bp_1, ymin = lower_1000bp_1), position = position_dodge(width =0.9), width=0.2) +
  scale_x_discrete() +
  theme_bw() +
  ggtitle("Estimation of migration rate from src to region using ace: 1,000 bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14))


# src to region: 10,000bp ----
quartz()
ggplot(ace_CIs, aes(x = mig_rate, y = median_10000bp_1, colour = tips)) +
  geom_point(size = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymax = upper_10000bp_1, ymin = lower_10000bp_1), position = position_dodge(width =0.9), width=0.2) +
  scale_x_discrete() +
  theme_bw() +
  ggtitle("Estimation of migration rate from src to region using ace: 10,000 bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14))


# region to src: 1,000bp ----
quartz()
ggplot(ace_CIs, aes(x = mig_rate, y = median_1000bp_2, colour = tips)) +
  geom_point(size = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymax = upper_1000bp_2, ymin = lower_1000bp_2), position = position_dodge(width =0.9), width=0.2) +
  scale_x_discrete() +
  theme_bw() +
  ggtitle("Estimation of migration rate from region to src using ace: 1,000 bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14))


# region to src: 10,000bp ----
quartz()
ggplot(ace_CIs, aes(x = mig_rate, y = median_10000bp_2, colour = tips)) +
  geom_point(size = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymax = upper_10000bp_2, ymin = lower_10000bp_2), position = position_dodge(width =0.9), width=0.2) +
  scale_x_discrete() +
  theme_bw() +
  ggtitle("Estimation of migration rate from region to src using ace: 10,000 bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14))



