#plot the credible interval for migration rates
library(ggplot2)

#read file with the confidence interval
ace_CIs <- readRDS("Analysis_SD/ape_ace/summary/confidence_interval/ace_confidence_intervals.RDS")
ace_CIs$mig_rate <- as.factor(ace_CIs$mig_rate)

ace_CIs$tips <- factor(ace_CIs$tips, levels = c("region100global100",
                                                "region300global300",
                                                "region1000global100",
                                                "region1000global500",
                                                "region1000global1000"))

ace_CIs["sample_size"] <- unlist(lapply(ace_CIs$tips, function(x) sum(as.numeric(str_extract_all(x, "\\d+")[[1]]))))
ace_CIs$sample_size <- as.factor(ace_CIs$sample_size)

#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sample_size_2000 <- subset(ace_CIs, sample_size == "2000")


quartz()

# src to region: 1,000bp ----
ggplot(ace_CIs, aes(x = mig_rate, y = median_1000bp_1, colour = sample_size)) +
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymax = upper_1000bp_1, ymin = lower_1000bp_1), position = position_dodge(width = 0.2), width=0.2) +
  scale_x_discrete() +
  scale_colour_manual(values = cbbPalette[c(2:4,8,1)],
                      name = "Sample size") +
  theme_bw() +
  ggtitle("Estimation of migration rate from src to region using ace: partial pol gene") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14), legend.position = "bottom")


# src to region: 10,000bp ----
quartz()
ggplot(ace_CIs, aes(x = mig_rate, y = median_10000bp_1, colour = sample_size)) +
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymax = upper_10000bp_1, ymin = lower_10000bp_1), position = position_dodge(width = 0.2), width = 0.2) +
  scale_x_discrete() +
  scale_colour_manual(values = cbbPalette[c(2:4,8,1)],
                      name = "Sample size") +
  theme_bw() +
  ggtitle("Estimation of migration rate from src to region using ace: complete HIV genome") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14), legend.position = "bottom")


# region to src: 1,000bp ----
quartz()
ggplot(ace_CIs, aes(x = mig_rate, y = median_1000bp_2, colour = sample_size)) +
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymax = upper_1000bp_2, ymin = lower_1000bp_2), position = position_dodge(width = 0.2), width=0.2) +
  scale_x_discrete() +
  scale_colour_manual(values = cbbPalette[c(2:4,8,1)],
                      name = "Sample size") +
  theme_bw() +
  ggtitle("Estimation of migration rate from region to src using ace: partial pol gene") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14), legend.position = "bottom")


# region to src: 10,000bp ----
quartz()
ggplot(ace_CIs, aes(x = mig_rate, y = median_10000bp_2, colour = sample_size)) +
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymax = upper_10000bp_2, ymin = lower_10000bp_2), position = position_dodge(width = 0.2), width=0.2) +
  scale_x_discrete() +
  scale_colour_manual(values = cbbPalette[c(2:4,8,1)],
                      name = "Sample size") +
  theme_bw() +
  ggtitle("Estimation of migration rate from region to src using ace: complete HIV genome") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14), legend.position = "bottom")




# sample size = 2000 only ----
ggplot(sample_size_2000, aes(x = mig_rate, y = median_1000bp_1, colour = sample_size)) +
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymax = upper_1000bp_1, ymin = lower_1000bp_1), position = position_dodge(width = 0.2), width=0.2) +
  scale_x_discrete() +
  scale_colour_manual(values = cbbPalette[c(1)],
                      name = "Sample size") +
  theme_bw() +
  ggtitle("Estimation of migration rate from src to region using ace: 1,000 bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14), legend.position = "bottom")


ggplot(sample_size_2000, aes(x = mig_rate, y = median_10000bp_1, colour = sample_size)) +
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymax = upper_10000bp_1, ymin = lower_10000bp_1), position = position_dodge(width = 0.2), width=0.2) +
  scale_x_discrete() +
  scale_colour_manual(values = cbbPalette[c(1)],
                      name = "Sample size") +
  theme_bw() +
  ggtitle("Estimation of migration rate from src to region using ace: 10,000 bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Migration rate") +
  theme(text = element_text(size=14), legend.position = "bottom")
