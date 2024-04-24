library(tracerer)

# Obtain an example log file its name
filename <- "~/Desktop/beast2_results/importation_rate/medium/region100global100/run1/results/final/stage2_1/beast_phydyn_strictClock.log"

filename <- "~/Desktop/beast1_results/importation_rate/exponential/medium/region100global100/run1/results/run_1/phylogeography_beast1.log"

# Parse that log file
beast_log_full <- parse_beast_tracelog_file(filename)

teste <- mcmc(data = beast_log_full[,2:ncol(beast_log_full)])
effectiveSize(teste)

# Remove the burn-in
beast_log <- remove_burn_ins(
  beast_log_full,
  burn_in_fraction = 0.1
)

teste <- mcmc(data = beast_log[,2:ncol(beast_log)])
effectiveSize(teste)

# Calculates the effective sample sizes of all parameter estimates
esses <- calc_esses(beast_log, sample_interval = 1)
