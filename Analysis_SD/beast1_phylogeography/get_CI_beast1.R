#get the credible interval for migration rate from src to I
library(tracerer)
library(stringr)


# Obtain an example log file its name
mcmc_results1 <- list.files("~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast1/importation_rate/low/region100global100/run1",
                   recursive = TRUE, full.names = TRUE)

mcmc_logs <- mcmc_results1[grepl("combined_runs123456.log", mcmc_results1)]
length(mcmc_logs)

all_CIs <- data.frame()

# Parse that log file
for(i in 1:length(mcmc_logs)){

  print(i)
  print(mcmc_logs[i])

  texts <- str_split(mcmc_logs[i], "/")

  migration <- texts[[1]][10]
  tips <- texts[[1]][11]
  run <- texts[[1]][12]
  sim <- str_split(texts[[1]][14], "_")[[1]][2]
  seqlen <- texts[[1]][13]


  if(seqlen == "results"){

    seqlen <- "1000bp"

  } else{

    seqlen <- "10000bp"

  }

  beast_log <- parse_beast_tracelog_file(mcmc_logs[i])
  names(beast_log)[1] <- "Sample"

  sum_stats <- quantile(beast_log$location.rates.src.I, probs = c(0.025, 0.5, 0.975))
  lower <- unname(sum_stats[1])
  median <- unname(sum_stats[2])
  upper <- unname(sum_stats[3])

  CIs <- data.frame(migration = migration,
                    tips = tips,
                    run = run,
                    sim = sim,
                    seqlen = seqlen,
                    upper = upper,
                    median = median,
                    lower = lower)

  all_CIs <- rbind(all_CIs, CIs)


}




