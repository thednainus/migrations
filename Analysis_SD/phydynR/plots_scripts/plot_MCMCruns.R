#plot MCMC runs as violin plots for a couple of runs only

library(ggplot2)
library(data.table)

#get the data

#low migration rate
low_allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_all_tree_0.03_1000bp_MCMCcombined.RDS")
low_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_mh1990_0.03_1000bp_MCMCcombined.RDS")
low_allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_all_tree_0.03_10000bp_MCMCcombined.RDS")
low_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_mh1990_0.03_10000bp_MCMCcombined.RDS")


#medium migration rate
medium_allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_all_tree_0.1_1000bp_MCMCcombined.RDS")
medium_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_mh1990_0.1_1000bp_MCMCcombined.RDS")
medium_allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_all_tree_0.1_10000bp_MCMCcombined.RDS")
medium_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_mh1990_0.1_10000bp_MCMCcombined.RDS")

#high migration rate
high_allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_all_tree_0.33_1000bp_MCMCcombined.RDS")
high_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_mh1990_0.33_1000bp_MCMCcombined.RDS")
high_allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_all_tree_0.33_10000bp_MCMCcombined.RDS")
high_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/data_for_violoin_plots/2reps_combinedRuns/region1000global500_mh1990_0.33_10000bp_MCMCcombined.RDS")

mcmc_MCMCcombined <- rbind(low_allTree_1000bp, low_mh1990_1000bp,
                   low_allTree_10000bp, low_mh1990_10000bp,
                   medium_allTree_1000bp, medium_mh1990_1000bp,
                   medium_allTree_10000bp, medium_mh1990_10000bp,
                   high_allTree_1000bp, high_mh1990_1000bp,
                   high_allTree_10000bp, high_mh1990_10000bp)
mcmc_MCMCcombined$mig_rate <- as.character(mcmc_MCMCcombined$mig_rate)
mcmc_MCMCcombined$mig_rate <- as.factor(mcmc_MCMCcombined$mig_rate)
mcmc_MCMCcombined$rep <- as.factor(mcmc_MCMCcombined$rep)
mcmc_MCMCcombined["treeData_migRate"] <- paste(mcmc_MCMCcombined$tree_data, mcmc_MCMCcombined$mig_rate,
                                       sep = "_")
mcmc_MCMCcombined["treeData_rep"] <- paste(mcmc_MCMCcombined$tree_data, mcmc_MCMCcombined$rep,
                                               sep = "_")

unique(mcmc_MCMCcombined$rep)

mcmc_MCMCcombined <- subset(mcmc_MCMCcombined, rep == 24)

#violin plot ----
quartz()
ggplot(mcmc_MCMCcombined, aes(x = mig_rate, y = combined_runs, fill = tree_data, color = tree_data )) +
  geom_violin(position=position_dodge(), draw_quantiles=c(0.5)) +
  #geom_boxplot(width=0.1, position = position_dodge(width =0.9), color="black") +
  theme_bw() +
  geom_hline(yintercept=c(0.03,0.1,0.33), linetype = "dotted") +
  ggtitle("Coverage: region 1000; global 500") +
  xlab("True value of migration rate") +
  ylab("Estimated migration rates") +
  theme(text = element_text(size=14)) +
  facet_wrap(   ~ seqlen, ncol = 2)
