#check ESS for parameters for combined logs
library(tracerer)

# Obtain an example log file its name
filenames <- list.files(path = "~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast1/importation_rate/medium/region100global100",
                        all.files = TRUE, pattern = "combined_runs*",
                        full.names = TRUE, recursive = TRUE)

# Parse that log file
for(i in 1:length(filenames)){

  print(filenames[i])
  beast_log_full <- parse_beast_tracelog_file(filenames[i])


  # Remove the burn-in
  beast_log <- remove_burn_ins(
    beast_log_full,
    burn_in_fraction = 0.1
  )

  # Calculates the effective sample sizes of all parameter estimates
  names(beast_log)[1] <- "Sample"
  esses <- calc_esses(beast_log, sample_interval = 1)
  print(esses)

}




