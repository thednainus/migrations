#plot the credible interval for migration rates
library(ggplot2)

#get all files with the data
#migration rate = 0.03
mig0.03allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_all_tree_0.03_1000bp_combined.RDS")
mig0.03mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_mh1990_0.03_1000bp_combined.RDS")
mig0.03allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_all_tree_0.03_10000bp_combined.RDS")
mig0.03mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_mh1990_0.03_10000bp_combined.RDS")


#migration rate = 0.1
mig0.1allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_all_tree_0.1_1000bp_combined.RDS")
mig0.1mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_mh1990_0.1_1000bp_combined.RDS")
mig0.1allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_all_tree_0.1_10000bp_combined.RDS")
mig0.1mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_mh1990_0.1_10000bp_combined.RDS")


#migration rate = 0.33
mig0.33allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_all_tree_0.33_1000bp_combined.RDS")
mig0.33mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_mh1990_0.33_1000bp_combined.RDS")
mig0.33allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_all_tree_0.33_10000bp_combined.RDS")
mig0.33mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/credible_intervals/combined_runs/region1000global500_mh1990_0.33_10000bp_combined.RDS")


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
rep_order <- as.character(sort(as.numeric(unique(seqlen1000bp$replicate))))
seqlen1000bp$replicate <- factor(seqlen1000bp$replicate, levels=rep_order)

seqlen10000bp <- subset(all_data, seqlen == "10000bp")
rep_order10000bp <- as.character(sort(as.numeric(unique(seqlen10000bp$replicate))))
seqlen10000bp$replicate <- factor(seqlen10000bp$replicate, levels=rep_order10000bp)


#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

quartz()
ggplot(seqlen1000bp, aes(x = replicate, y = median, color = treeData_migRate)) +
  geom_point(size = 1, position= position_dodge(0.8)) +
  geom_errorbar(aes(ymax = upper, ymin = lower),
                width = 1, position= position_dodge(0.8)) +
  geom_hline(yintercept = 0.03, linetype="dotted") +
  geom_hline(yintercept = 0.1, linetype="dotted") +
  geom_hline(yintercept = 0.33, linetype="dotted") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_colour_manual(values = cbbPalette[c(2,8,3,7,4,6)],
                      name="Tree data: mig. rate",
                      breaks=c("all_tree_0.03", "mh1990_0.03",
                               "all_tree_0.1", "mh1990_0.1",
                               "all_tree_0.33", "mh1990_0.33"),
                      labels=c("whole tree: 0.03", "partial tree: 0.03",
                               "whole tree: 0.1", "partial tree: 0.1",
                               "whole tree: 0.33", "partial tree: 0.33")) +
  theme_bw() +
  ggtitle("Sample size: region 1000 and global 500") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size = 14), legend.position = "bottom")


quartz()
ggplot(seqlen10000bp, aes(x = replicate, y = median, color = treeData_migRate)) +
  geom_point(size = 1, position= position_dodge(0.8)) +
  geom_errorbar(aes(ymax = upper, ymin = lower),
                width = 1, position= position_dodge(0.8)) +
  geom_hline(yintercept = 0.03, linetype="dotted") +
  geom_hline(yintercept = 0.1, linetype="dotted") +
  geom_hline(yintercept = 0.33, linetype="dotted") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_colour_manual(values = cbbPalette[c(2,8,3,7,4,6)],
                      name="Tree data: mig. rate",
                      breaks=c("all_tree_0.03", "mh1990_0.03",
                               "all_tree_0.1", "mh1990_0.1",
                               "all_tree_0.33", "mh1990_0.33"),
                      labels=c("whole tree: 0.03", "partial tree: 0.03",
                               "whole tree: 0.1", "partial tree: 0.1",
                               "whole tree: 0.33", "partial tree: 0.33")) +
  theme_bw() +
  ggtitle("Sample size: region 1000 and global 500") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size = 14), legend.position = "bottom")





