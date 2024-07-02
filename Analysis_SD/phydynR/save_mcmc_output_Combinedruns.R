#reminder on how to use coda to check MCMC samples
library(coda)
library(BayesianTools)
library(dplyr)
library(stringr)
library(ggplot2)

#get the data that was completed i = 800 iterations

#low/region1000global500/all_tree/run1/results used runs 3 and 17; for others 4 and 17

#list the name of files with results
#run1
mcmc_results1 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/low/region300global300/all_tree/run1/results_10000bp", recursive = TRUE, full.names = TRUE)
#run2
mcmc_results2 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/low/region300global300/all_tree/run2/results_10000bp", recursive = TRUE, full.names = TRUE)

#run1
#4 and 17
mcmc_results_iter.data1 <- mcmc_results1[grepl("iter.rdata", mcmc_results1)]
mcmc_results_iter.data1 <- mcmc_results_iter.data1[c(4,17)]
length(mcmc_results_iter.data1)
mcmc_results_out1 <- mcmc_results1[grepl("out_sim.RDS", mcmc_results1)]
mcmc_results_out1 <- mcmc_results_out1[c(4,17)]
length(mcmc_results_out1)

#run2
mcmc_results_iter.data2 <- mcmc_results2[grepl("iter.rdata", mcmc_results2)]
mcmc_results_iter.data2 <- mcmc_results_iter.data2[c(4,17)]
length(mcmc_results_iter.data2)

mcmc_results_out2 <- mcmc_results2[grepl("out_sim.RDS", mcmc_results2)]
mcmc_results_out2 <- mcmc_results_out2[c(4,17)]
length(mcmc_results_out2)


all_runs <- tibble()
for(j in 1:length(mcmc_results_iter.data1)){

  load(mcmc_results_iter.data1[j])
  print(i)

  if(i == 801){

    print(j)
    texts <- str_split(mcmc_results_out1[j], "/")
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


    out1 <- readRDS(mcmc_results_out1[j])
    out2 <- readRDS(mcmc_results_out2[j])

    mcmc_combined <- createMcmcSamplerList(list(out1, out2))
    print(gelmanDiagnostics(mcmc_combined, plot = FALSE))
    mcmc_combined_sample <- BayesianTools::getSample(mcmc_combined,
                                                     start=5000,
                                                     end = 24000,
                                                     coda = TRUE)

    #convert to coda
    #quartz()
    migrate1 <- as.data.frame(as.array(mcmc_combined_sample[,3]))
    migrate1["mean_chains"] <- unlist(apply(migrate1, 1, mean))


    runs <- data.frame(combined_runs = migrate1$mean_chains,
                       tips, tree_data, mig_rate, seqlen,
                       run_index = j,
                       rep = rep)

    all_runs <- rbind(all_runs, runs)
  }
}


filename <- paste(all_runs$tips[1], all_runs$tree_data[1], all_runs$mig_rate[1],
                  all_runs$seqlen[1], sep = "_")
print(filename)
filename <- paste(filename, "_MCMCcombined.RDS", sep = "")
saveRDS(all_runs, filename)






ggplot(teste_data_l, aes(x=variable, y=value)) +
  geom_violin()
