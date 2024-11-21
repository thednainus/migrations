#check ESS for parameters for combined logs
library(tracerer)


# Obtain an example log file its name
mcmc_results1 <- list.files("~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast1/importation_rate/high/region300global300/run1",
                   recursive = TRUE, full.names = TRUE)

mcmc_logs <- mcmc_results1[grepl("combined_runs123456.log", mcmc_results1)]
length(mcmc_logs)


# Parse that log file
for(i in 1:length(mcmc_logs)){

  print(i)

  print(mcmc_logs[i])
  beast_log_full <- parse_beast_tracelog_file(mcmc_logs[i])


  # Remove the burn-in
  #beast_log <- remove_burn_ins(
  #  beast_log_full,
  #  burn_in_fraction = 0.1
  #)

  beast_log <- beast_log_full

  # Calculates the effective sample sizes of all parameter estimates
  names(beast_log)[1] <- "Sample"
  esses <- calc_esses(beast_log, sample_interval = 1)
  index <- which(!is.na(esses))
  print("ALL ESS")
  print(esses[index])
  print("LOW ESS")
  ess_index <- which(esses < 200)
  print(esses[ess_index])
}




