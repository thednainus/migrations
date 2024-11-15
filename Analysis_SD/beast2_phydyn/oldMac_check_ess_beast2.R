#check ESS for parameters for combined logs
library(tracerer)

# Obtain an example log file its name
mcmc_results1 <- list.files("~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast2/importation_rate/low/region100global100/run1/results",
                            recursive = TRUE, full.names = TRUE)

mcmc_logs <- mcmc_results1[grepl("combined_runs12.log", mcmc_results1)]
length(mcmc_logs)

# Parse that log file
for(i in 1:length(mcmc_logs)){

  print(i)

  print(mcmc_logs[i])
  beast_log_full <- parse_beast_tracelog_file(mcmc_logs[i])


  # Remove the burn-in
  beast_log <- remove_burn_ins(
    beast_log_full,
    burn_in_fraction = 0.1
  )

  # Calculates the effective sample sizes of all parameter estimates
  esses <- calc_esses(beast_log, sample_interval = 1)
  ess_index <- which(esses < 200)
  print(esses[ess_index])


}




