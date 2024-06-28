#precision: the difference between the upper and lower interval of the
# 95% credible interval divided by the true value
#coverage: how often among the replicates per parameter value does the credible
#interval contains the simulated true value
#relative error: shows the significance of an error by comparing it to the
#original error
library(dplyr)
library(stringr)

#get the data that was completed i = 800 iterations

CIs <- list.files("Analysis_SD/phydynR/mcmc_results/summary/credible_intervals/combined_runs", recursive = TRUE, full.names = TRUE)


for(j in 1:length(CIs)){

  CI <- readRDS(CIs[j])
  if(unique(CI$mig_rate) == 0.03){

    CI["mig_rate"] <- 1/30

  }

  if(unique(CI$mig_rate) == 0.33){

    CI["mig_rate"] <- 1/3

  }

  if(unique(CI$mig_rate) == 0.1){

    CI["mig_rate"] <- 1/10

  }

  #precision from combined runs

  prec <- (CI$upper - CI$lower)/unique(CI$mig_rate)


  #coverage for combined runs

  CI["within_range"] <- ifelse(unique(CI$mig_rate) >= CI$lower  &
                                  unique(CI$mig_rate) <= CI$upper  , "yes", "no")
  coverage <- sum(CI$within_range == "yes")/nrow(CI)


  #relative error for combined runs

  relative_error <- abs((CI$median - unique(CI$mig_rate))/unique(CI$mig_rate))

  all_stats <- tibble(total_reps = nrow(CI),
                      tips = unique(CI$tips),
                      seqlen = unique(CI$seqlen),
                      mig_rate = unique(CI$mig_rate),
                      tree_data = unique(CI$tree_data),
                      prec = prec,
                      coverage = coverage,
                      relative_error = relative_error)

  filename <- paste(CI$tips[1], CI$tree_data[1], CI$mig_rate[1],
                    CI$seqlen[1], "stats", sep = "_")
  filename <- paste(filename, "_combined.RDS", sep = "")
  print(filename)
  saveRDS(all_stats, filename)
}




