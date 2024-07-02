#reminder on how to use coda to check MCMC samples
library(coda)
library(BayesianTools)
library(dplyr)
library(stringr)
library(ggplot2)

#get the data that was completed i = 800 iterations

#list the name of files with results
#run1
mcmc_results1 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/high/region300global300/mh1990/run1/results_10000bp", recursive = TRUE, full.names = TRUE)
#run2
mcmc_results2 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/high/region300global300/mh1990/run2/results_10000bp", recursive = TRUE, full.names = TRUE)

#run1
mcmc_results_iter.data1 <- mcmc_results1[grepl("iter.rdata", mcmc_results1)]
mcmc_results_iter.data1 <- mcmc_results_iter.data1[-c(4,19,20,28,33,41,43)]
length(mcmc_results_iter.data1)
mcmc_results_out1 <- mcmc_results1[grepl("out_sim.RDS", mcmc_results1)]
mcmc_results_out1 <- mcmc_results_out1[-c(4,19,20,28,33,41,43)]
length(mcmc_results_out1)

#run2
mcmc_results_iter.data2 <- mcmc_results2[grepl("iter.rdata", mcmc_results2)]
mcmc_results_iter.data2 <- mcmc_results_iter.data2[-c(4,19,20,28,33,41,43)]
length(mcmc_results_iter.data2)

mcmc_results_out2 <- mcmc_results2[grepl("out_sim.RDS", mcmc_results2)]
mcmc_results_out2 <- mcmc_results_out2[-c(4,19,20,28,33,41,43)]
length(mcmc_results_out2)



CI_all <- tibble()

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

    #convert to coda
    #convert to coda
    mcmc_combined <- createMcmcSamplerList(list(out1, out2))
    mcmc_combined_sample <- BayesianTools::getSample(mcmc_combined, start=5000, end = 24000, coda = TRUE)

    #for combined runs
    summary_results <- summary(mcmc_combined_sample)
    lower <- summary_results$quantiles[3,1]
    median <- summary_results$quantiles[3,3]
    upper <- summary_results$quantiles[3,5]

    quantiles_results <- tibble(index = j,
                                tips = tips,
                                seqlen = seqlen,
                                replicate = rep,
                                mig_rate = mig_rate,
                                tree_data = tree_data,
                                lower = lower,
                                median = median,
                                upper = upper)

    CI_all <- rbind(CI_all, quantiles_results)
  }
}

#View(CI_all)

filename <- paste(CI_all$tips[1], CI_all$tree_data[1], CI_all$mig_rate[1],
                  CI_all$seqlen[1], sep = "_")
print(filename)
filename <- paste(filename, "_combined.RDS", sep = "")
saveRDS(CI_all, filename)


