library(tracerer)
library(dplyr)
library(stringr)
library(ggplot2)

# get log files
mcmc_results1 <- list.files("~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast1/importation_rate/medium/region100global100/run1",
                            recursive = TRUE, full.names = TRUE)

location_logs <- mcmc_results1[grepl("combined_location_runs123456.log", mcmc_results1)]
location_logs <- location_logs[c(4,6)]

length(location_logs)



all_runs <- tibble()
for(j in 1:length(location_logs)){


  print(j)

  texts <- str_split(location_logs[j], "/")
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

  beast_log_full <- parse_beast_tracelog_file(location_logs[j])


  # Remove the burn-in
  beast_log <- remove_burn_ins(
    beast_log_full,
    burn_in_fraction = 0.3
  )


    #convert to coda
    #quartz()
    migrate1 <- as.data.frame(beast_log$location.rates.src.I)
    names(migrate1) <- "location.rates.src.I"


    runs <- data.frame(combined_runs = migrate1,
                       tips, mig_rate, seqlen,
                       run_index = j,
                       rep = rep)

    all_runs <- rbind(all_runs, runs)
}


filename <- paste(all_runs$tips[1], all_runs$mig_rate[1],
                  all_runs$seqlen[1], sep = "_")
print(filename)
filename <- paste(filename, "_MCMCcombined_beast1.RDS", sep = "")
saveRDS(all_runs, filename)






ggplot(teste_data_l, aes(x=variable, y=value)) +
  geom_violin()
