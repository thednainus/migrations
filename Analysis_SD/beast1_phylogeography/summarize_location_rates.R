library(tracerer)
library(stringr)
library(dplyr)



# get log files
mcmc_results1 <- list.files("~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast1/importation_rate/high/region100global100/run1",
                            recursive = TRUE, full.names = TRUE)

location_logs <- mcmc_results1[grepl("combined_location_runs123456.log", mcmc_results1)]
location_logs <- location_logs[-c(1,7,13,14,15,17,31,41,43,46,49)]

length(location_logs)

CI_all <- tibble()

for(i in 1:length(location_logs)){

  print(i)

  texts <- str_split(location_logs[i], "/")
  tips <- texts[[1]][11]
  seqlen <- texts[[1]][13]
  if(seqlen == "results"){
    seqlen <- "1000bp"
  }else{
    seqlen <- "10000bp"
  }

  rep <- str_split(texts[[1]][14], "_")[[1]][2]
  mig_rate <- texts[[1]][10]
  if(mig_rate == "medium"){
    mig_rate <-  0.1
  }else if(mig_rate == "low"){
    mig_rate <-  0.33
  }else if(mig_rate == "high"){
    mig_rate <-  0.03
  }


  print(location_logs[i])
  beast_log_full <- parse_beast_tracelog_file(location_logs[i])


  # Remove the burn-in
  beast_log <- remove_burn_ins(
    beast_log_full,
    burn_in_fraction = 0.3
  )



  #for combined runs
  summary_results <- quantile(beast_log$location.rates.src.I, probs =c(0.025,0.5,0.975))
  lower <- unname(summary_results[1])
  median <- unname(summary_results[2])
  upper <- unname(summary_results[3])

  quantiles_results <- tibble(index = i,
                              tips = tips,
                              seqlen = seqlen,
                              replicate = rep,
                              mig_rate = mig_rate,
                              lower = lower,
                              median = median,
                              upper = upper)

  CI_all <- rbind(CI_all, quantiles_results)

}


filename <- paste(CI_all$tips[1], CI_all$mig_rate[1],
                  CI_all$seqlen[1], sep = "_")
print(filename)
filename <- paste(filename, "_combined_beast1.RDS", sep = "")
saveRDS(CI_all, filename)
