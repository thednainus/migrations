#get the average stepes that each run in the MCMC was carried out for
library(tracerer)
library(stringr)



# Obtain an example log file its name
mcmc_results1 <- list.files("~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast1/importation_rate/high/region100global100/run4/results_10000bp",
                   recursive = TRUE, full.names = TRUE)


mcmc_logs <- mcmc_results1[grepl("phylogeography_beast1.log", mcmc_results1)]
length(mcmc_logs)

all_states <- data.frame()


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


  beast_log_full <- parse_beast_tracelog_file(mcmc_logs[i])
  total_steps <- tail(beast_log_full, n = 1)[,1]
  logEvery <- beast_log_full[3,1] - beast_log_full[2,1]


  info_steps <- data.frame(migration = migration,
                           tips = tips,
                           run = run,
                           sim = sim,
                           seqlen = seqlen,
                           total_steps = total_steps,
                           logEvery = logEvery)

  all_states <- rbind(all_states, info_steps)


}

all_states
mean(all_states[,6])
mean(all_states[,7])
nrow(all_states)

