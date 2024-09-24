# Save MCMC runs for individual runs and removing the burn-in


library(coda)
library(BayesianTools)
library(dplyr)
library(stringr)
library(ggplot2)

#get the data that was completed i = 800 iterations

#list the name of files with results
#run1
MCMC_results1 <- list.files("Analysis_SD/phydynR/MCMC_results/ICL_cluster/importation_rate/low/region1000global500/mh1990/run1/results_10000bp", recursive = TRUE, full.names = TRUE)
#run2
MCMC_results2 <- list.files("Analysis_SD/phydynR/MCMC_results/ICL_cluster/importation_rate/low/region1000global500/mh1990/run2/results_10000bp", recursive = TRUE, full.names = TRUE)

#run1
MCMC_results_iter.data1 <- MCMC_results1[grepl("iter.rdata", MCMC_results1)]
MCMC_results_iter.data1 <- MCMC_results_iter.data1[-c(8,25,31,38)]
length(MCMC_results_iter.data1)
MCMC_results_out1 <- MCMC_results1[grepl("out_sim.RDS", MCMC_results1)]
MCMC_results_out1 <- MCMC_results_out1[-c(8,25,31,38)]
length(MCMC_results_out1)

#run2
MCMC_results_iter.data2 <- MCMC_results2[grepl("iter.rdata", MCMC_results2)]
MCMC_results_iter.data2 <- MCMC_results_iter.data2[-c(8,25,31,38)]
length(MCMC_results_iter.data2)

MCMC_results_out2 <- MCMC_results2[grepl("out_sim.RDS", MCMC_results2)]
MCMC_results_out2 <- MCMC_results_out2[-c(8,25,31,38)]
length(MCMC_results_out2)


all_runs <- tibble()
for(j in 1:length(MCMC_results_iter.data1)){

  load(MCMC_results_iter.data1[j])
  print(i)

  if(i == 801){

    print(j)
    texts <- str_split(MCMC_results_out1[j], "/")
    tips <- texts[[1]][7]
    seqlen <- texts[[1]][10]
    if(seqlen == "results"){
      seqlen <- "1000bp"
    }else{
      seqlen <- "10000bp"
    }

    rep <- str_split(texts[[1]][11], "_")[[1]][2]
    mig_rate <- texts[[1]][6]
    if(mig_rate == "medium"){
      mig_rate <-  0.1
    }else if(mig_rate == "low"){
      mig_rate <-  0.33
    }else if(mig_rate == "high"){
      mig_rate <-  0.03
    }
    tree_data <- texts[[1]][8]


    out1 <- readRDS(MCMC_results_out1[j])

    #convert to coda
    #quartz()
    out1_coda <- BayesianTools::getSample(out1, start = 5000, coda = TRUE)
    migrate1 <- as.data.frame(as.array(out1_coda[,3]))
    migrate1["mean_chains"] <- unlist(apply(migrate1, 1, mean))


    runs <- data.frame(run1 = migrate1$mean_chains,
                       tips, tree_data, mig_rate, seqlen,
                       run_index = j,
                       rep = rep)

    all_runs <- rbind(all_runs, runs)
  }
}


filename <- paste(all_runs$tips[1], all_runs$tree_data[1], all_runs$mig_rate[1],
                  all_runs$seqlen[1], sep = "_")
print(filename)
filename <- paste(filename, "_run1.RDS", sep = "")
saveRDS(all_runs, filename)
