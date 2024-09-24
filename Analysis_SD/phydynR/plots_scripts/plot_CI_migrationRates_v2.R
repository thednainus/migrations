#plot the credible interval for migration rates
library(ggplot2)

#get all files with the data
mig0.03allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_all_tree_0.03_1000bp_combined.RDS")
mig0.03mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_mh1990_0.03_10000bp_combined.RDS")
mig0.03allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_all_tree_0.03_10000bp_combined.RDS")
mig0.03mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_mh1990_0.03_10000bp_combined.RDS")



mig0.1allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_all_tree_0.1_1000bp_combined.RDS")
mig0.1mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_mh1990_0.1_1000bp_combined.RDS")
mig0.1allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_all_tree_0.1_10000bp_combined.RDS")
mig0.1mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_mh1990_0.1_10000bp_combined.RDS")



mig0.33allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_all_tree_0.33_1000bp_combined.RDS")
mig0.33mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_mh1990_0.33_1000bp_combined.RDS")
mig0.33allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_all_tree_0.33_10000bp_combined.RDS")
mig0.33mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global100_mh1990_0.33_10000bp_combined.RDS")


all_data <- rbind(mig0.03mh1990_1000bp, mig0.03allTree_1000bp,
                  mig0.03mh1990_10000bp, mig0.03allTree_10000bp,
                  mig0.1mh1990_1000bp, mig0.1allTree_1000bp,
                  mig0.1mh1990_10000bp, mig0.1allTree_10000bp,
                  mig0.33mh1990_1000bp, mig0.33allTree_1000bp,
                  mig0.33mh1990_10000bp, mig0.33allTree_10000bp)
all_data["treeData_migRate"] <- paste(all_data$tree_data,
                                      all_data$mig_rate,
                                      sep = "_")


seqlen1000bp <- subset(all_data, seqlen == "1000bp")

quartz()


ggplot(seqlen1000bp, aes(x = replicate, y = median1, color = treeData_migRate)) +
  geom_point(size = 1, position= position_dodge(0.5)) +
  geom_errorbar(aes(ymax = upper1, ymin = lower1), position= position_dodge(0.5)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  ggtitle("95% credible interval") +
  ylab("Median and 2.5% and 97.5% quantiles for migration rate") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))





