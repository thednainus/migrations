#precision: the difference between the upper and lower interval of the
# 95% credible interval divided by the true value
#coverage: how often among the replicates per parameter value does the credible
#interval contains the simulated true value
#relative error: shows the significance of an error by comparing it to the
#original error
library(dplyr)
library(stringr)

#get the data that was completed i = 800 iterations

CIs <- list.files("Analysis_SD/phydynR/mcmc_results/summary/credible_intervals", recursive = TRUE, full.names = TRUE)


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

  #precision from run1 and run2

  prec1 <- (CI$upper1 - CI$lower1)/unique(CI$mig_rate)
  prec2 <- (CI$upper2 - CI$lower2)/unique(CI$mig_rate)


  #coverage for run1 and run2

  CI["within_range1"] <- ifelse(CI$lower1 >= unique(CI$mig_rate) |
                                 unique(CI$mig_rate) <= CI$upper1, "yes", "no")
  coverage1 <- sum(CI$within_range1 == "yes")/nrow(CI)

  CI["within_range2"] <- ifelse(CI$lower2 >= unique(CI$mig_rate) |
                                  unique(CI$mig_rate) <= CI$upper2, "yes", "no")
  coverage2 <- sum(CI$within_range2 == "yes")/nrow(CI)

  #relative error for run1 and run2

  mean_relative_error1 <- abs((CI$median1 - unique(CI$mig_rate))/unique(CI$mig_rate))
  mean_relative_error2 <- abs((CI$median2 - unique(CI$mig_rate))/unique(CI$mig_rate))

  all_stats <- tibble(total_reps = nrow(CI),
                      tips = unique(CI$tips),
                      seqlen = unique(CI$seqlen),
                      mig_rate = unique(CI$mig_rate),
                      tree_data = unique(CI$tree_data),
                      prec1 = prec1,
                      prec2 = prec2,
                      coverage1 = coverage1,
                      coverage2 = coverage2,
                      mean_relative_error1 = mean_relative_error1,
                      mean_relative_error2 = mean_relative_error2)

  filename <- paste(CI$tips[1], CI$tree_data[1], CI$mig_rate[1],
                    CI$seqlen[1], "stats", sep = "_")
  filename <- paste(filename, ".RDS", sep = "")
  print(filename)
  saveRDS(all_stats, filename)
}




