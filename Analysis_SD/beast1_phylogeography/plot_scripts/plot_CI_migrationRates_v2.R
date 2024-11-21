#plot the credible interval for migration rates
library(ggplot2)

#get all files with the data
#region 100 global 100
low_migs <- readRDS("Analysis_SD/beast1_phylogeography/results_summary/credible_intervals/region100global100_low_1000bp_beast1_combinedRuns.RDS")

medium_migs <- readRDS("Analysis_SD/beast1_phylogeography/results_summary/credible_intervals/region100global100_medium_1000bp_beast1_combinedRuns.RDS")

high_migs <- readRDS("Analysis_SD/beast1_phylogeography/results_summary/credible_intervals/region100global100_high_1000bp_beast1_combinedRuns.RDS")



all_data <- rbind(low_migs, medium_migs, high_migs)
all_data["treeData_migRate"] <- paste(all_data$tree_data,
                                      all_data$mig_rate,
                                      sep = "_")
all_data["seqlen_text"] <- ifelse(all_data$seqlen == "1000bp",
                                  "partial pol gene",
                                  "complete HIV genome")




#color blind save pallet (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

quartz()
ggplot(all_data, aes(x = sim, y = median, color = migration)) +
  geom_point(size = 1, position= position_dodge(0.8)) +
  geom_errorbar(aes(ymax = upper, ymin = lower),
                width = 1, position= position_dodge(0.8)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_grid(~ seqlen_text, scales = "free") +
  scale_colour_manual(values = cbbPalette[c(3,4,7)]) +
  theme_bw() +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size = 14), legend.position = "bottom")


