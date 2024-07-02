#plot MCMC runs as violin plots
library(ggplot2)
library(data.table)

#get the data


#low migration rate
low_allTree_1000bp <- readRDS("Analysis_SD/beast1_phylogeography/mcmc_results/summary/data_for_violin_plots/2reps_combinedRuns/region100global100_0.03_1000bp_MCMCcombined_beast1.RDS")
#low_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/2reps_combinedRuns/region300global300_all_tree_0.03_10000bp_MCMCcombined.RDS")


#medium migration rate
medium_allTree_1000bp <- readRDS("Analysis_SD/beast1_phylogeography/mcmc_results/summary/data_for_violin_plots/2reps_combinedRuns/region100global100_0.1_1000bp_MCMCcombined_beast1.RDS")
#medium_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/2reps_combinedRuns/region300global300_all_tree_0.1_10000bp_MCMCcombined.RDS")

#high migration rate
high_allTree_1000bp <- readRDS("Analysis_SD/beast1_phylogeography/mcmc_results/summary/data_for_violin_plots/2reps_combinedRuns/region100global100_0.33_1000bp_MCMCcombined_beast1.RDS")
#high_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/2reps_combinedRuns/region300global300_all_tree_0.33_10000bp_MCMCcombined.RDS")

mcmc_MCMCcombined <- rbind(low_allTree_1000bp,
                   medium_allTree_1000bp,
                   high_allTree_1000bp)
mcmc_MCMCcombined$mig_rate <- as.character(mcmc_MCMCcombined$mig_rate)
mcmc_MCMCcombined$mig_rate <- as.factor(mcmc_MCMCcombined$mig_rate)
mcmc_MCMCcombined$rep <- as.factor(mcmc_MCMCcombined$rep)


#violin plot ----
quartz()
ggplot(mcmc_MCMCcombined, aes(x=mig_rate, y=location.rates.src.I, colour = rep )) +
  geom_violin(position=position_dodge(), draw_quantiles=c(0.5)) +
  #geom_boxplot(width=0.1, position = position_dodge(width =0.9), color="black") +
  theme_bw() +
  ggtitle("region 100; global 100 (beast1 phylogeography results for partial pol alignments)") +
  xlab("True value of migration rate") +
  ylab("Estimated migration rates") +
  theme(text = element_text(size=14))



