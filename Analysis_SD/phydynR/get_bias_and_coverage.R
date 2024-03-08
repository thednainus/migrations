#bias: take the average of all the estimates and subtract from the true mean
#coverage: how often among the replicates per parameter value does the credible
#interval contains the simulated true value
library(coda)
library(BayesianTools)
library(dplyr)
library(stringr)
library(ggplot2)

#get the data that was completed i = 800 iterations

CIs <- list.files("Analysis_SD/phydynR/mcmc_results/summary", recursive = TRUE, full.names = TRUE)



bias_coverage_all <- tibble()

for(j in 1:length(CIs)){

  CI <- readRDS(CIs[j])
  if(unique(CI$mig_rate) == 0.03){

    CI["mig_rate"] <- 1/30

  }

  #bias from run1 and run2

  bias1 <- mean(CI$median1)/unique(CI$mig_rate)
  bias2 <- mean(CI$median2)/unique(CI$mig_rate)
  total_rep <- nrow(CI)

  #coverage for run1 and run2

  CI["within_range1"] <- ifelse(CI$lower1 >= unique(CI$mig_rate) |
                                 unique(CI$mig_rate) <= CI$upper1, "yes", "no")
  coverage1 <- sum(CI$within_range1 == "yes")/nrow(CI)

  CI["within_range2"] <- ifelse(CI$lower2 >= unique(CI$mig_rate) |
                                  unique(CI$mig_rate) <= CI$upper2, "yes", "no")
  coverage2 <- sum(CI$within_range2 == "yes")/nrow(CI)

  #relative error for run1 and run2

  mean_relative_error1 <- mean(abs((CI$median1 - unique(CI$mig_rate))/unique(CI$mig_rate)))
  mean_relative_error2 <- mean(abs((CI$median2 - unique(CI$mig_rate))/unique(CI$mig_rate)))

  all_stats <- tibble(tips = unique(CI$tips),
                      seqlen = unique(CI$seqlen),
                      mig_rate = unique(CI$mig_rate),
                      tree_data = unique(CI$tree_data),
                      bias1 = bias1,
                      bias2 = bias2,
                      coverage1 = coverage1,
                      coverage2 = coverage2,
                      mean_relative_error1 = mean_relative_error1,
                      mean_relative_error2 = mean_relative_error2)


}

View(CI_all)

filename <- paste(CI_all$tips[1], CI_all$tree_data[1], CI_all$mig_rate[1],
                  CI_all$seqlen[1], sep = "_")
print(filename)
filename <- paste(filename, ".RDS", sep = "")
saveRDS(CI_all, filename)






CI_all_sorted <- CI_all[order(CI_all$median1),]
CI_all_sorted["order_rep"] <- 1:nrow(CI_all_sorted)
CI_all_sorted$order_rep <- as.factor(CI_all_sorted$order_rep)


prior <- rlnorm(10000,log(1/20), 1/4)
prior_mig <- quantile(prior, probs = c(0.25, 0.5, 0.975))

prior_mig_df <- data.frame(lower = prior_mig[1],
                           median = prior_mig[2],
                           upper = prior_mig[3],
                           order_rep = 50)
prior_mig_df$order_rep <- as.factor(prior_mig_df$order_rep)

quartz()


ggplot(CI_all_sorted, aes(x = order_rep, y = median1)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upper1, ymin = lower1)) +
  geom_hline(yintercept=0.33, linetype="dashed") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  ggtitle("Credible interval for posterior: 1,000 region tips and 100 global tips: 1,000bp (all_tree)") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))


quartz()

ggplot(CI_all_sorted, aes(x = order_rep, y = median)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_point(data = prior_mig_df, aes(x = order_rep, y = median), size = 2) +
  geom_errorbar(data = prior_mig_df, ymax = upper, ymin = lower) +
  geom_hline(yintercept=0.1, linetype="dashed") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  ggtitle("Credible interval for posterior: 1,000 region tips and 500 global tips: 9,719bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))



CI_all$replicate <- as.factor(CI_all$replicate)

quartz()
ggplot(CI_all, aes(x = replicate, y = median)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_hline(yintercept=0.1, linetype="dashed") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  ggtitle("Credible interval for posterior: 1,000 region tips and 500 global tips: 9,719bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))

