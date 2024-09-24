# Analysis carried out for individual runs

# plot coverage, precision and relative error for MCMC analyses
# for the analyses using the whole tree as maximum height
# and for analyzing part of the tree only
# for more details check out ?phydynR::colik

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)

#get the data

#low migration rate
low_allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_all_tree_0.0333333333333333_1000bp_stats.RDS")
low_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_mh1990_0.0333333333333333_1000bp_stats.RDS")
low_allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_all_tree_0.0333333333333333_10000bp_stats.RDS")
low_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_mh1990_0.0333333333333333_10000bp_stats.RDS")


#medium migration rate
medium_allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_all_tree_0.1_1000bp_stats.RDS")
medium_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_mh1990_0.1_1000bp_stats.RDS")
medium_allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_all_tree_0.1_10000bp_stats.RDS")
medium_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_mh1990_0.1_10000bp_stats.RDS")

#high migration rate
high_allTree_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_all_tree_0.333333333333333_1000bp_stats.RDS")
high_mh1990_1000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_mh1990_0.333333333333333_1000bp_stats.RDS")
high_allTree_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_all_tree_0.333333333333333_10000bp_stats.RDS")
high_mh1990_10000bp <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/region1000global100_mh1990_0.333333333333333_10000bp_stats.RDS")




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
coverage <- distinct(coverage)

#plot coverage
quartz()
ggplot(coverage, aes(x = mig_rate, y = coverage1)) +
  geom_point(aes(colour = tree_data, shape = tree_data)) +
  geom_hline(yintercept=1, linetype="dotted") +
  facet_wrap(  ~ seqlen, ncol = 2) +
  theme_bw() +
  ggtitle("Coverage: region 1000; global 100") +
  xlab("Migration rate") +
  theme(text = element_text(size=14))


# Precision ----

precision <- stats[,c(1:7)]
precision["group"] <- paste(precision$total_reps,
                                 precision$tips,
                                 precision$seqlen,
                                 precision$mig_rate,
                                 precision$tree_data, sep = "_")
precision_quantiles <- precision %>%
  group_by(group) %>%
  summarize(lower1 = quantile(prec1, probs = 0.025),
            median1 = quantile(prec1, probs = 0.5),
            upper1 = quantile(prec1, probs = 0.975),
            lower2 = quantile(prec2, probs = 0.025),
            median2 = quantile(prec2, probs = 0.5),
            upper2 = quantile(prec2, probs = 0.975))


precision_quantiles["seqlen"] <- unlist(lapply(precision_quantiles$group, function(x) str_split(x, "_")[[1]][3]))

precision_quantiles["tree_data"] <- unlist(lapply(precision_quantiles$group, function(x) str_split(x, "_")[[1]][5]))
precision_quantiles$tree_data[precision_quantiles$tree_data == "all"] <- "all_tree"

precision_quantiles["mig_rate"] <- unlist(lapply(precision_quantiles$group, function(x) str_split(x, "_")[[1]][4]))



quartz()
ggplot(precision_quantiles, aes(x = mig_rate)) +
  geom_point(aes(y = median1, colour = tree_data), size = 1, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymax = upper1, ymin = lower1, width = 0.1, colour = tree_data), position= position_dodge(0.5)) +
  facet_wrap(  ~ seqlen, ncol = 2) +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_bw() +
  ggtitle("Precision: region 1000; global 500") +
  xlab("Migration rate") +
  theme(text = element_text(size=14))



# Relative error ----

relative_error <- stats[,c(1:5,10:11)]
relative_error["group"] <- paste(relative_error$total_reps,
                                 relative_error$tips,
                                 relative_error$seqlen,
                                 relative_error$mig_rate,
                                 relative_error$tree_data, sep = "_")
relative_error_quantiles <- relative_error %>%
  group_by(group) %>%
  summarize(lower1 = quantile(mean_relative_error1, probs = 0.025),
            median1 = quantile(mean_relative_error1, probs = 0.5),
            upper1 = quantile(mean_relative_error1, probs = 0.975),
            lower2 = quantile(mean_relative_error2, probs = 0.025),
            median2 = quantile(mean_relative_error2, probs = 0.5),
            upper2 = quantile(mean_relative_error2, probs = 0.975))


relative_error_quantiles["seqlen"] <- unlist(lapply(relative_error_quantiles$group, function(x) str_split(x, "_")[[1]][3]))

relative_error_quantiles["tree_data"] <- unlist(lapply(relative_error_quantiles$group, function(x) str_split(x, "_")[[1]][5]))
relative_error_quantiles$tree_data[relative_error_quantiles$tree_data == "all"] <- "all_tree"

relative_error_quantiles["mig_rate"] <- unlist(lapply(relative_error_quantiles$group, function(x) str_split(x, "_")[[1]][4]))


quartz()
ggplot(relative_error_quantiles, aes(x = mig_rate)) +
  geom_point(aes(y = median1, colour = tree_data), size = 1, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymax = upper1, ymin = lower1, colour = tree_data, width = 0.1), position = position_dodge(0.5)) +
  facet_wrap( ~ seqlen, ncol = 2) +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_bw() +
  ggtitle("Relative Error: region 1000; global 500") +
  xlab("Migration rate") +
  theme(text = element_text(size=14))
