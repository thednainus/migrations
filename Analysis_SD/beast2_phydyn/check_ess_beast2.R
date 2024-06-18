#check ESS for parameters for combined logs
library(tracerer)

# Obtain an example log file its name
filenames <- list.files(path = "~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast2/importation_rate/medium/region100global100",
                        all.files = TRUE, pattern = "beast_phydyn_strictClock.log",
                        full.names = TRUE, recursive = TRUE)

run1 <- filenames[grepl("run1", filenames)]
run2 <- filenames[grepl("run2", filenames)]

# Parse that log file
for(i in 1:length(filenames)){

  print(filenames[i])
  beast_log_full <- parse_beast_tracelog_file(filenames[i])


  # Remove the burn-in
  beast_log <- remove_burn_ins(
    beast_log_full,
    burn_in_fraction = 0.2
  )

  # Calculates the effective sample sizes of all parameter estimates
  esses <- calc_esses(beast_log, sample_interval = 100)
  print(esses)

}




