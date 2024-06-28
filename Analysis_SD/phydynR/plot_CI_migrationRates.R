#plot the credible interval for migration rates
library(ggplot2)

#get all files with the data
#data_files <- list.files("CI_migrationRates_results", full.names = TRUE)
data_files <- list.files("Analysis_SD/phydynR/mcmc_results/summary/credible_intervals/combined_runs", full.names = TRUE)

#merge data in each file
CIs <- tibble()
for(i in 1:length(data_files)){

  CI_all <- readRDS(data_files[i])

  CI_all_sorted <- CI_all[order(CI_all$median),]
  CI_all_sorted["order_rep"] <- 1:nrow(CI_all_sorted)
  CI_all_sorted$order_rep <- as.factor(CI_all_sorted$order_rep)

  CIs <- rbind(CIs, CI_all_sorted)

}


mig0.33mh1990 <- subset(CIs, mig_rate == 0.33 & tree_data == "mh1990")
mig0.33allTree <- subset(CIs, mig_rate == 0.33 & tree_data == "all_tree")
mig0.1mh1990 <- subset(CIs, mig_rate == 0.1 & tree_data == "mh1990")
mig0.1allTree <- subset(CIs, mig_rate == 0.1 & tree_data == "all_tree")
mig0.03mh1990 <- subset(CIs, mig_rate == 0.03 & tree_data == "mh1990")
mig0.03allTree <- subset(CIs, mig_rate == 0.03 & tree_data == "all_tree")


quartz()


ggplot(mig0.33mh1990, aes(x = order_rep, y = median)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_hline(yintercept=0.33, linetype="twodash") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap( tips ~ seqlen, scales = "free", ncol = 2) +
  theme_bw() +
  ggtitle("Credible interval for posterior: maximum height = 1990") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))


ggplot(mig0.33allTree, aes(x = order_rep, y = median)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_hline(yintercept=0.33, linetype="twodash") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap( tips ~ seqlen, scales = "free", ncol = 2) +
  theme_bw() +
  ggtitle("Credible interval for posterior: using all the tree") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))


ggplot(mig0.1mh1990, aes(x = order_rep, y = median)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_hline(yintercept=0.1, linetype="twodash") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap( tips ~ seqlen, scales = "free", ncol = 2) +
  theme_bw() +
  ggtitle("Credible interval for posterior: maximum height = 1990") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))


ggplot(mig0.1allTree, aes(x = order_rep, y = median)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_hline(yintercept=0.1, linetype="twodash") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap( tips ~ seqlen, scales = "free", ncol = 2) +
  theme_bw() +
  ggtitle("Credible interval for posterior: using all the tree") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))



ggplot(mig0.03mh1990, aes(x = order_rep, y = median)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_hline(yintercept=0.03, linetype="twodash") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap( tips ~ seqlen, scales = "free", ncol = 2) +
  theme_bw() +
  ggtitle("Credible interval for posterior: maximum height = 1990") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))


ggplot(mig0.03allTree, aes(x = order_rep, y = median)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_hline(yintercept=0.03, linetype="twodash") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_wrap( tips ~ seqlen, scales = "free", ncol = 2) +
  theme_bw() +
  ggtitle("Credible interval for posterior: using all the tree") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))
