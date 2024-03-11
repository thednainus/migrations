#plot coverage, precision and relative error for phydynR analyses
library(data.table)

#get the data

#low migration rate
low_allTree_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_all_tree_0.0333333333333333_1000bp_stats.RDS")
low_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_mh1990_0.0333333333333333_1000bp_stats.RDS")
low_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_all_tree_0.0333333333333333_1000bp_stats.RDS")
low_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_mh1990_0.0333333333333333_10000bp_stats.RDS")


#medium migration rate
medium_allTree_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_all_tree_0.1_1000bp_stats.RDS")
medium_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_mh1990_0.1_1000bp_stats.RDS")
medium_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_all_tree_0.1_1000bp_stats.RDS")
medium_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_mh1990_0.1_10000bp_stats.RDS")

#high migration rate
high_allTree_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_all_tree_0.333333333333333_1000bp_stats.RDS")
high_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_mh1990_0.333333333333333_1000bp_stats.RDS")
high_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_all_tree_0.333333333333333_1000bp_stats.RDS")
high_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/region1000global100_mh1990_0.333333333333333_10000bp_stats.RDS")




stats <- rbind(low_allTree_1000bp, low_mh1990_1000bp,
               low_allTree_10000bp, low_mh1990_10000bp,
               medium_allTree_1000bp, medium_mh1990_1000bp,
               medium_allTree_10000bp, medium_mh1990_10000bp,
               high_allTree_1000bp, high_mh1990_1000bp,
               high_allTree_10000bp, high_mh1990_10000bp)
stats["mig_rate"] <- unlist(lapply(stats$mig_rate, function(x) round(x, 2)))
stats$mig_rate <- as.character(stats$mig_rate)
stats$mig_rate <- as.factor(stats$mig_rate)


#coverage ----

coverage <- stats[,c(1:5,8:9)]
coverage_m <- melt(coverage, id.vars=c("total_reps", "tips", "seqlen", "mig_rate",
                                 "tree_data"))


#plot coverage
quartz()
ggplot(coverage_m, aes(x = mig_rate, y = value, colour = variable)) +
  geom_point() +
  facet_wrap( tree_data ~ seqlen, ncol = 3) +
  theme_bw() +
  ggtitle("Coverage") +
  xlab("Migration rate") +
  theme(text = element_text(size=14))




ggplot(coverage_m, aes(x = mig_rate, y = value)) +
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
