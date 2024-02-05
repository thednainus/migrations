#reminder on how to use coda to check MCMC samples
library(coda)
library(BayesianTools)
library(dplyr)
library(stringr)
library(ggplot2)

#get the data that was completed i = 800 iterations

#list the name of files with results
mcmc_results <- list.files("Analysis_SD/phydynR/mcmc_results/QMUL_cluster/region1000global100/results_10000bp", recursive = TRUE, full.names = TRUE)
mcmc_results <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/region1000global500_maxHeight/results_10000bp", recursive = TRUE, full.names = TRUE)
mcmc_results_iter.data <- mcmc_results[grepl("iter.rdata", mcmc_results)]
#for 1,000bp
#mcmc_results_iter.data <- mcmc_results_iter.data[-c(4,12,15,40,43,46,58,89,94)]
#for 10,000bp
mcmc_results_iter.data <- mcmc_results_iter.data[-c(9,21,41)]
mcmc_results_iter.data <- mcmc_results_iter.data[-c(1:51)]

mcmc_results_out <- mcmc_results[grepl("out_sim.RDS", mcmc_results)]
#for 10,000bp
mcmc_results_out <- mcmc_results_out[-c(9,21,41)]
mcmc_results_out <- mcmc_results_out[-c(1:51)]
#for 1,000 bp
#mcmc_results_out <- mcmc_results_out[-c(4,12,15,40,43,46,58,89,94)]

CI_all <- tibble()

for(j in 1:length(mcmc_results_iter.data)){

  load(mcmc_results_iter.data[j])
  print(i)

  if(i == 501){

    print(j)
    texts <- str_split(mcmc_results_out[j], "/")
    tips <- texts[[1]][5]
    #tips <- texts[[1]][6]
    seqlen <- texts[[1]][6]
    #seqlen <- texts[[1]][7]
    rep <- str_split(texts[[1]][7], "_")[[1]][2]
    #rep <- str_split(texts[[1]][9], "_")[[1]][2]

    out <- readRDS(mcmc_results_out[j])
    #out1 <- getSample(out, start = 7500)
    #summary(out1)

    #convert to coda
    out_coda <- BayesianTools::getSample(out, start = 7000, coda = TRUE)
    #quartz()
    #plot(out_coda, ask = TRUE)

    summary_results <- summary(out_coda)
    lower <- summary_results$quantiles[3,2]
    median <- summary_results$quantiles[3,3]
    upper <- summary_results$quantiles[3,5]
    quantiles_results <- tibble(tips = tips,
                                seqlen = seqlen,
                                replicate = rep,
                                lower_src1980 = summary_results$quantiles[1,2],
                                median_src1980 = summary_results$quantiles[1,3],
                                upper_src1980 = summary_results$quantiles[1,5],
                                lower_I1980 = summary_results$quantiles[2,2],
                                median_I1980 = summary_results$quantiles[2,3],
                                upper_I1980 = summary_results$quantiles[2,5],
                                lower_srcMigrationRate = summary_results$quantiles[3,2],
                                median_srcMigrationRate = summary_results$quantiles[3,3],
                                upper_srcMigrationRate = summary_results$quantiles[3,5],
                                lower_alpha = summary_results$quantiles[4,2],
                                median_alpha = summary_results$quantiles[4,3],
                                upper_alpha = summary_results$quantiles[4,5],
                                lower_beta1980 = summary_results$quantiles[5,2],
                                median_beta1980 = summary_results$quantiles[5,3],
                                upper_beta1980 = summary_results$quantiles[5,5],
                                lower_beta1995 = summary_results$quantiles[6,2],
                                median_beta1995 = summary_results$quantiles[6,3],
                                upper_beta1995 = summary_results$quantiles[6,5],
                                lower_beta2005 = summary_results$quantiles[7,2],
                                median_beta2005 = summary_results$quantiles[7,3],
                                upper_beta2005 = summary_results$quantiles[7,5])

    CI_all <- rbind(CI_all, quantiles_results)



  }




}

library(ggplot2)
library(reshape2)

CI_all_long <- melt(CI_all, id.vars=c("tips", "seqlen", "replicate"))
CI_all_long["quantile"] <- unlist(lapply(CI_all_long$variable, function(x) str_split(x, "_")[[1]][1]))
CI_all_long["parameters"] <- unlist(lapply(CI_all_long$variable, function(x) str_split(x, "_")[[1]][2]))
CI_all_long2 <- CI_all_long[,c(3,6,7,5)]
CI_all_wide <-  dcast(CI_all_long2, replicate + parameters ~ quantile, value.var="value")

quartz()

ggplot(CI_all_wide, aes(x = replicate)) +
  geom_point(aes(y=median), size = 1) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  facet_wrap(parameters ~ ., ncol = 2, scales = "free") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  ggtitle("Credible interval for posterior: 1,000 region tips and 500 global tips: 9,719bp") +
  ylab("Median and 25% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))



