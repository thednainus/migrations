#reminder on how to use coda to check MCMC samples
library(dplyr)
library(stringr)



#list ace results
ace_results <- list.files("Analysis_SD/ape_ace/summary", recursive = TRUE, full.names = TRUE)

ace_results <- ace_results[-1]


CI_all <- tibble()

for(j in 1:length(ace_results)){
  print(j)
  results <- readRDS(ace_results[j])
  results_1000bp <- subset(results, seqlen == "1000bp")
  results_10000bp <- subset(results, seqlen == "10000bp")

  tips <- unique(results$tips)
  mig_rate <- unique(results$mig_rate)

  #subset by location (1 is from src to region)
  results_1000bp_1 <- subset(results_1000bp, location == 1)
  results_10000bp_1 <- subset(results_10000bp, location == 1)

  #subset by location (2 is from region to src which should be zero)
  results_1000bp_2 <- subset(results_1000bp, location == 2)
  results_10000bp_2 <- subset(results_10000bp, location == 2)

  #get quantiles
  summary_1000bp_1 <- quantile(results_1000bp_1$rates, probs=c(0.025, 0.5, 0.975))
  lower_1000bp_1 <- unname(summary_1000bp_1[1])
  median_1000bp_1 <- unname(summary_1000bp_1[2])
  upper_1000bp_1 <- unname(summary_1000bp_1[3])

  summary_10000bp_1 <- quantile(results_10000bp_1$rates, probs=c(0.025, 0.5, 0.975))
  lower_10000bp_1 <- unname(summary_10000bp_1[1])
  median_10000bp_1 <- unname(summary_10000bp_1[2])
  upper_10000bp_1 <- unname(summary_10000bp_1[3])

  summary_1000bp_2 <- quantile(results_1000bp_2$rates, probs=c(0.025, 0.5, 0.975))
  lower_1000bp_2 <- unname(summary_1000bp_2[1])
  median_1000bp_2 <- unname(summary_1000bp_2[2])
  upper_1000bp_2 <- unname(summary_1000bp_2[3])

  summary_10000bp_2 <- quantile(results_10000bp_2$rates, probs=c(0.025, 0.5, 0.975))
  lower_10000bp_2 <- unname(summary_10000bp_2[1])
  median_10000bp_2 <- unname(summary_10000bp_2[2])
  upper_10000bp_2 <- unname(summary_10000bp_2[3])



  quantiles_results <- tibble(index = j,
                              tips = tips,
                              mig_rate = mig_rate,
                              lower_1000bp_1 = lower_1000bp_1,
                              median_1000bp_1 = median_1000bp_1,
                              upper_1000bp_1 = upper_1000bp_1,
                              lower_1000bp_2 = lower_1000bp_2,
                              median_1000bp_2 = median_1000bp_2,
                              upper_1000bp_2 = upper_1000bp_2,
                              lower_10000bp_1 = lower_10000bp_1,
                              median_10000bp_1 = median_10000bp_1,
                              upper_10000bp_1 = upper_10000bp_1,
                              lower_10000bp_2 = lower_10000bp_2,
                              median_10000bp_2 = median_10000bp_2,
                              upper_10000bp_2 = upper_10000bp_2
                              )

  CI_all <- rbind(CI_all, quantiles_results)
}

#View(CI_all)

filename <- "ace_confidence_intervals.RDS"
saveRDS(CI_all, filename)


