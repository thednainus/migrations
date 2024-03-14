#plot MCMC runs as violin plots
library(ggplo2)
library(data.table)

#get the data


#low migration rate
low_allTree_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_all_tree_0.03_1000bp_run1.RDS")
low_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_mh1990_0.03_1000bp_run1.RDS")
low_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_all_tree_0.03_10000bp_run1.RDS")
low_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_mh1990_0.03_10000bp_run1.RDS")


#medium migration rate
medium_allTree_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_all_tree_0.1_1000bp_run1.RDS")
medium_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_mh1990_0.1_1000bp_run1.RDS")
medium_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_all_tree_0.1_10000bp_run1.RDS")
medium_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_mh1990_0.1_10000bp_run1.RDS")

#high migration rate
high_allTree_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_all_tree_0.33_1000bp_run1.RDS")
high_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_mh1990_0.33_1000bp_run1.RDS")
high_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_all_tree_0.33_10000bp_run1.RDS")
high_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/data_for_violoin_plots/region1000global100_mh1990_0.33_10000bp_run1.RDS")

mcmc_run1 <- rbind(low_allTree_1000bp, low_mh1990_1000bp,
                   low_allTree_10000bp, low_mh1990_10000bp,
                   medium_allTree_1000bp, medium_mh1990_1000bp,
                   medium_allTree_10000bp, medium_mh1990_10000bp,
                   high_allTree_1000bp, high_mh1990_1000bp,
                   high_allTree_10000bp, high_mh1990_10000bp)
mcmc_run1$mig_rate <- as.character(mcmc_run1$mig_rate)
mcmc_run1$mig_rate <- as.factor(mcmc_run1$mig_rate)


#violin plot ----
quartz()
ggplot(mcmc_run1, aes(x=mig_rate, y=run1, fill = tree_data ,color = tree_data)) +
  geom_violin(position=position_dodge(), draw_quantiles=c(0.5)) +
  #geom_boxplot(width=0.1, position = position_dodge(width =0.9), color="black") +
  theme_bw() +
  ggtitle("Coverage: region 1000; global 100") +
  xlab("Migration rate") +
  theme(text = element_text(size=14)) +
  facet_wrap( ~ seqlen, ncol = 2)

