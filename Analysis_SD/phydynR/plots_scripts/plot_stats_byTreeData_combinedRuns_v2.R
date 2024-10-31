# Analysis carried out for individual the combined runs.

# plot coverage, precision and relative error for MCMC analyses
# for the analyses using the whole tree as maximum height
# and for analyzing part of the tree only
# for more details check out ?phydynR::colik


library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)

#get the data
#at = all trees
#mh = using maximum height = 1990
#pop1 = (region = 100 and global = 100 sequences)
#pop2 = (region = 300 and global = 300 sequences)
#pop3 = (region = 1000 and global = 100 sequences)
#pop4 = (region = 1000 and global = 500 sequences)

#low migration rate ----

#region = 100 and global = 100
low_at_1000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_all_tree_0.0333333333333333_1000bp_stats_combined.RDS")
low_mh_1000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_mh1990_0.0333333333333333_1000bp_stats_combined.RDS")
low_at_10000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_all_tree_0.0333333333333333_10000bp_stats_combined.RDS")
low_mh_10000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_mh1990_0.0333333333333333_10000bp_stats_combined.RDS")

#region = 300 and global = 300
low_at_1000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_all_tree_0.0333333333333333_1000bp_stats_combined.RDS")
low_mh_1000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_mh1990_0.0333333333333333_1000bp_stats_combined.RDS")
low_at_10000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_all_tree_0.0333333333333333_10000bp_stats_combined.RDS")
low_mh_10000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_mh1990_0.0333333333333333_10000bp_stats_combined.RDS")


#region = 1000 and global = 100
low_at_1000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_all_tree_0.0333333333333333_1000bp_stats_combined.RDS")
low_mh_1000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_mh1990_0.0333333333333333_1000bp_stats_combined.RDS")
low_at_10000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_all_tree_0.0333333333333333_10000bp_stats_combined.RDS")
low_mh_10000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_mh1990_0.0333333333333333_10000bp_stats_combined.RDS")

#region = 1000 and global = 500
low_at_1000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_all_tree_0.0333333333333333_1000bp_stats_combined.RDS")
low_mh_1000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_mh1990_0.0333333333333333_1000bp_stats_combined.RDS")
low_at_10000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_all_tree_0.0333333333333333_10000bp_stats_combined.RDS")
low_mh_10000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_mh1990_0.0333333333333333_10000bp_stats_combined.RDS")


#medium migration rate ----

#region = 100 and global = 100
medium_at_1000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_all_tree_0.1_1000bp_stats_combined.RDS")
medium_mh_1000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_mh1990_0.1_1000bp_stats_combined.RDS")
medium_at_10000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_all_tree_0.1_10000bp_stats_combined.RDS")
medium_mh_10000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_mh1990_0.1_10000bp_stats_combined.RDS")

#region = 300 and global = 300
medium_at_1000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_all_tree_0.1_1000bp_stats_combined.RDS")
medium_mh_1000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_mh1990_0.1_1000bp_stats_combined.RDS")
medium_at_10000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_all_tree_0.1_10000bp_stats_combined.RDS")
medium_mh_10000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_mh1990_0.1_10000bp_stats_combined.RDS")


#region = 1000 and global = 100
medium_at_1000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_all_tree_0.1_1000bp_stats_combined.RDS")
medium_mh_1000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_mh1990_0.1_1000bp_stats_combined.RDS")
medium_at_10000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_all_tree_0.1_10000bp_stats_combined.RDS")
medium_mh_10000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_mh1990_0.1_10000bp_stats_combined.RDS")

#region = 1000 and global = 500
medium_at_1000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_all_tree_0.1_1000bp_stats_combined.RDS")
medium_mh_1000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_mh1990_0.1_1000bp_stats_combined.RDS")
medium_at_10000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_all_tree_0.1_10000bp_stats_combined.RDS")
medium_mh_10000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_mh1990_0.1_10000bp_stats_combined.RDS")


#high migration rate ----

#region = 100 and global = 100
high_at_1000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_all_tree_0.333333333333333_1000bp_stats_combined.RDS")
high_mh_1000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_mh1990_0.333333333333333_1000bp_stats_combined.RDS")
high_at_10000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_all_tree_0.333333333333333_10000bp_stats_combined.RDS")
high_mh_10000bp_pop1 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region100global100_mh1990_0.333333333333333_10000bp_stats_combined.RDS")

#region = 300 and global = 300
high_at_1000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_all_tree_0.333333333333333_1000bp_stats_combined.RDS")
high_mh_1000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_mh1990_0.333333333333333_1000bp_stats_combined.RDS")
high_at_10000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_all_tree_0.333333333333333_10000bp_stats_combined.RDS")
high_mh_10000bp_pop2 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region300global300_mh1990_0.333333333333333_10000bp_stats_combined.RDS")


#region = 1000 and global = 100
high_at_1000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_all_tree_0.333333333333333_1000bp_stats_combined.RDS")
high_mh_1000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_mh1990_0.333333333333333_1000bp_stats_combined.RDS")
high_at_10000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_all_tree_0.333333333333333_10000bp_stats_combined.RDS")
high_mh_10000bp_pop3 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global100_mh1990_0.333333333333333_10000bp_stats_combined.RDS")

#region = 1000 and global = 500
high_at_1000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_all_tree_0.333333333333333_1000bp_stats_combined.RDS")
high_mh_1000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_mh1990_0.333333333333333_1000bp_stats_combined.RDS")
high_at_10000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_all_tree_0.333333333333333_10000bp_stats_combined.RDS")
high_mh_10000bp_pop4 <- readRDS("Analysis_SD/phydynR/MCMC_results/summary/stats/combined_runs/region1000global500_mh1990_0.333333333333333_10000bp_stats_combined.RDS")



stats <- rbind(low_at_1000bp_pop1, low_at_10000bp_pop1,
               low_mh_1000bp_pop1, low_mh_10000bp_pop1,
               low_at_1000bp_pop2, low_at_10000bp_pop2,
               low_mh_1000bp_pop2, low_mh_10000bp_pop2,
               low_at_1000bp_pop3, low_at_10000bp_pop3,
               low_mh_1000bp_pop3, low_mh_10000bp_pop3,
               low_at_1000bp_pop4, low_at_10000bp_pop4,
               low_mh_1000bp_pop4, low_mh_10000bp_pop4,
               medium_at_1000bp_pop1, medium_at_10000bp_pop1,
               medium_mh_1000bp_pop1, medium_mh_10000bp_pop1,
               medium_at_1000bp_pop2, medium_at_10000bp_pop2,
               medium_mh_1000bp_pop2, medium_mh_10000bp_pop2,
               medium_at_1000bp_pop3, medium_at_10000bp_pop3,
               medium_mh_1000bp_pop3, medium_mh_10000bp_pop3,
               medium_at_1000bp_pop4, medium_at_10000bp_pop4,
               medium_mh_1000bp_pop4, medium_mh_10000bp_pop4,
               high_at_1000bp_pop1, high_at_10000bp_pop1,
               high_mh_1000bp_pop1, high_mh_10000bp_pop1,
               high_at_1000bp_pop2, high_at_10000bp_pop2,
               high_mh_1000bp_pop2, high_mh_10000bp_pop2,
               high_at_1000bp_pop3, high_at_10000bp_pop3,
               high_mh_1000bp_pop3, high_mh_10000bp_pop3,
               high_at_1000bp_pop4, high_at_10000bp_pop4,
               high_mh_1000bp_pop4, high_mh_10000bp_pop4)

#plots for low migration rate ----

stats["sample_size"] <- unlist(lapply(stats$tips, function(x) sum(as.numeric(str_extract_all(x, "\\d+")[[1]]))))
stats["mig_rate"] <- unlist(lapply(stats$mig_rate, function(x) round(x, 2)))
stats$mig_rate <- as.character(stats$mig_rate)
stats$mig_rate <- as.factor(stats$mig_rate)
#stats["migration_seqlen"] <- paste(stats$mig_rate, stats$seqlen, sep = "/")
stats <- stats[,2:9]


#coverage ----

coverage <- stats[,c(1:4,6,8)]
coverage <- distinct(coverage)
#coverage_low_m <- melt(coverage_low, id.vars=c("tips",
#                                           "seqlen",
#                                           "mig_rate",
#                                           "tree_data",
#                                           "migration_seqlen"))
coverage["percentage"] <- coverage$coverage * 100
coverage$sample_size <- as.factor(coverage$sample_size )


#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#plot coverage
quartz()
ggplot(coverage, aes(x = mig_rate, y = percentage)) +
  geom_point(position=position_dodge(width = 0.5),
             aes(colour = sample_size), size = 3) +
  geom_hline(yintercept = 100, linetype="dotted") +
  facet_grid(seqlen ~ tree_data, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2:4,8)]) +
  theme_bw() +
  xlab("Migration rate") +
  ylab("Coverage") +
  theme(text = element_text(size = 20), legend.position = "bottom")




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

precision_quantiles["tips"] <- unlist(lapply(precision_quantiles$group, function(x) str_split(x, "_")[[1]][1]))
precision_quantiles["sample_size"] <- unlist(lapply(precision_quantiles$tips, function(x) sum(as.numeric(str_extract_all(x, "\\d+")[[1]]))))
precision_quantiles$sample_size <- as.factor(precision_quantiles$sample_size)




quartz()
ggplot(precision_quantiles, aes(x = mig_rate)) +
  geom_point(aes(y = median, colour = sample_size), size = 4,
             position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper, ymin = lower,
                    width = 0.5, colour = sample_size),
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype="dotted") +
  facet_grid(seqlen ~ tree_data, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2:4,8)]) +
  theme_bw() +
  xlab("Migration rate") +
  ylab("Precision") +
  theme(text = element_text(size = 20), legend.position = "bottom")



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
relative_error_quantiles["tips"] <- unlist(lapply(relative_error_quantiles$group, function(x) str_split(x, "_")[[1]][1]))
relative_error_quantiles["sample_size"] <- unlist(lapply(relative_error_quantiles$tips, function(x) sum(as.numeric(str_extract_all(x, "\\d+")[[1]]))))
relative_error_quantiles$sample_size <- as.factor(relative_error_quantiles$sample_size)


quartz()
ggplot(relative_error_quantiles, aes(x = mig_rate)) +
  geom_point(aes(y = median, colour = sample_size), size = 4,
             position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymax = upper, ymin = lower, width = 0.5, colour = sample_size),
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype="dotted") +
  facet_grid(seqlen ~ tree_data, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(2:4,8)]) +
  theme_bw() +
  xlab("Migration rate") +
  ylab("Relative error") +
  theme(text = element_text(size = 20), legend.position = "bottom")
