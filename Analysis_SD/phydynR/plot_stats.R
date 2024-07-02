#plot coverage, precision and relative error for phydynR analyses
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)

#get the data

#low migration rate
low_allTree_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_all_tree_0.0333333333333333_1000bp_stats_combined.RDS")
low_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_mh1990_0.0333333333333333_1000bp_stats_combined.RDS")
low_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_all_tree_0.0333333333333333_10000bp_stats_combined.RDS")
low_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_mh1990_0.0333333333333333_10000bp_stats_combined.RDS")


#medium migration rate
medium_allTree_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_all_tree_0.1_1000bp_stats_combined.RDS")
medium_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_mh1990_0.1_1000bp_stats_combined.RDS")
medium_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_all_tree_0.1_10000bp_stats_combined.RDS")
medium_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_mh1990_0.1_10000bp_stats_combined.RDS")

#high migration rate
high_allTree_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_all_tree_0.333333333333333_1000bp_stats_combined.RDS")
high_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_mh1990_0.333333333333333_1000bp_stats_combined.RDS")
high_allTree_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_all_tree_0.333333333333333_10000bp_stats_combined.RDS")
high_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/mcmc_results/summary/stats/combined_runs/region300global300_mh1990_0.333333333333333_10000bp_stats_combined.RDS")




stats <- rbind(low_allTree_1000bp, low_mh1990_1000bp,
               low_allTree_10000bp, low_mh1990_10000bp,
               medium_allTree_1000bp, medium_mh1990_1000bp,
               medium_allTree_10000bp, medium_mh1990_10000bp,
               high_allTree_1000bp, high_mh1990_1000bp,
               high_allTree_10000bp, high_mh1990_10000bp)
stats["mig_rate"] <- unlist(lapply(stats$mig_rate, function(x) round(x, 2)))
stats$mig_rate <- as.character(stats$mig_rate)
stats$mig_rate <- as.factor(stats$mig_rate)
stats["migration_seqlen"] <- paste(stats$mig_rate, stats$seqlen, sep = "/")
stats <- stats[,2:9]


#coverage ----

coverage <- stats[,c(1:4,6,8)]
coverage <- distinct(coverage)
coverage_m <- melt(coverage, id.vars=c("tips", "seqlen", "mig_rate", "tree_data", "migration_seqlen"))
coverage_m["value2"] <- coverage_m$value * 100


#plot coverage
quartz()
ggplot(coverage_m, aes(x = migration_seqlen, y = value2)) +
  geom_point(aes(colour = tree_data, shape = tree_data), size = 3) +
  geom_hline(yintercept=100, linetype="dotted") +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = -15)) +
  ggtitle("Coverage: region 300; global 300") +
  xlab("Migration rate / sequence length") +
  ylab("Coverage") +
  theme(text = element_text(size=20))


# Precision ----

precision <- stats[,c(1:5)]
precision["group"] <- paste(precision$tips,
                            precision$seqlen,
                            precision$mig_rate,
                            precision$tree_data, sep = "_")
precision_quantiles <- precision %>%
  group_by(group) %>%
  summarize(lower = quantile(prec, probs = 0.025),
            median = quantile(prec, probs = 0.5),
            upper = quantile(prec, probs = 0.975))


precision_quantiles["seqlen"] <- unlist(lapply(precision_quantiles$group, function(x) str_split(x, "_")[[1]][2]))

precision_quantiles["tree_data"] <- unlist(lapply(precision_quantiles$group, function(x) str_split(x, "_")[[1]][4]))
precision_quantiles$tree_data[precision_quantiles$tree_data == "all"] <- "all_tree"

precision_quantiles["mig_rate"] <- unlist(lapply(precision_quantiles$group, function(x) str_split(x, "_")[[1]][3]))
precision_quantiles["migration_seqlen"] <- paste(precision_quantiles$mig_rate,
                                                 precision_quantiles$seqlen,
                                                 sep = "/")


quartz()
ggplot(precision_quantiles, aes(x = migration_seqlen)) +
  geom_point(aes(y = median, colour = tree_data), size = 4, position = position_dodge(width =0.7)) +
  geom_errorbar(aes(ymax = upper, ymin = lower, width = 0.5, colour =tree_data), position = position_dodge(width =0.7)) +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = -15)) +
  ggtitle("Precision: region 300; global 300") +
  xlab("Migration rate / sequence length") +
  ylab("Precision") +
  theme(text = element_text(size=20))



# Relative error ----

relative_error <- stats[,c(1:4,7:8)]
relative_error["group"] <- paste(relative_error$tips,
                                 relative_error$seqlen,
                                 relative_error$mig_rate,
                                 relative_error$tree_data, sep = "_")
relative_error_quantiles <- relative_error %>%
  group_by(group) %>%
  summarize(lower = quantile(relative_error, probs = 0.025),
            median = quantile(relative_error, probs = 0.5),
            upper = quantile(relative_error, probs = 0.975))


relative_error_quantiles["seqlen"] <- unlist(lapply(relative_error_quantiles$group, function(x) str_split(x, "_")[[1]][2]))

relative_error_quantiles["tree_data"] <- unlist(lapply(relative_error_quantiles$group, function(x) str_split(x, "_")[[1]][4]))
relative_error_quantiles$tree_data[relative_error_quantiles$tree_data == "all"] <- "all_tree"

relative_error_quantiles["mig_rate"] <- unlist(lapply(relative_error_quantiles$group, function(x) str_split(x, "_")[[1]][3]))
relative_error_quantiles["migration_seqlen"] <- paste(relative_error_quantiles$mig_rate,
                                                      relative_error_quantiles$seqlen,
                                                      sep = "/")


quartz()
ggplot(relative_error_quantiles, aes(x = migration_seqlen)) +
  geom_point(aes(y = median, colour = tree_data), size = 4, position = position_dodge(width =0.7)) +
  geom_errorbar(aes(ymax = upper, ymin = lower, width = 0.5, colour =tree_data), position = position_dodge(width =0.7)) +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = -15)) +
  ggtitle("Relative error: region 300; global 300") +
  xlab("Migration rate / sequence length") +
  ylab("Relative error") +
  theme(text = element_text(size=20))
