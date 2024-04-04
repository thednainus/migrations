#estimate the Gelman statistics to access convergence of the independent runs
library(coda)
library(BayesianTools)
library(stringr)
library(dplyr)

#get the data that was completed i = 800 iterations

#list the name of files with results
mcmc_results1 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/low/region1000global500/mh1990/run1/results_10000bp", recursive = TRUE, full.names = TRUE)
mcmc_results2 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/low/region1000global500/mh1990/run2/results_10000bp", recursive = TRUE, full.names = TRUE)

mcmc_results_iter.data1 <- mcmc_results1[grepl("iter.rdata", mcmc_results1)]
mcmc_results_iter.data1 <- mcmc_results_iter.data1[-c(15,2)]
length(mcmc_results_iter.data1)
mcmc_results_out1 <- mcmc_results1[grepl("out_sim.RDS", mcmc_results1)]
mcmc_results_out1 <- mcmc_results_out1[-c(15,2)]
length(mcmc_results_out1)

mcmc_results_iter.data2 <- mcmc_results2[grepl("iter.rdata", mcmc_results2)]
mcmc_results_iter.data2 <- mcmc_results_iter.data2[-c(15,2)]
length(mcmc_results_iter.data2)
mcmc_results_out2 <- mcmc_results2[grepl("out_sim.RDS", mcmc_results2)]
mcmc_results_out2 <- mcmc_results_out2[-c(15,2)]
length(mcmc_results_out2)

allESS <- tibble()

for(j in 1:length(mcmc_results_iter.data1)){

  run_number1 <- str_split(mcmc_results_iter.data1[j], "/")[[1]][11]
  run_number2 <- str_split(mcmc_results_iter.data2[j], "/")[[1]][11]

  if(run_number1 != run_number2){

    run_number1 <- str_split(run_number1, "_")[[1]][2]
    run_number2 <- str_split(run_number2, "_")[[1]][2]


  }

  if(run_number1 == run_number2){

    load(mcmc_results_iter.data1[j])
    print(i)
    i1 <- i

    load(mcmc_results_iter.data2[j])
    print(i)
    i2 <- i

    #then it will
    if(i1 == i2 & i1 == 801){


      print(j)
      mcmc_results_iter.data1[j]
      mcmc_results_iter.data2[j]


      out1 <- readRDS(mcmc_results_out1[j])
      out2 <- readRDS(mcmc_results_out2[j])

      #convert to coda
      mcmc_combined <- createMcmcSamplerList(list(out1, out2))
      mcmc_combined_sample <- BayesianTools::getSample(mcmc_combined, start=5000, coda = TRUE)


      #get ESS values
      ESS <- coda::effectiveSize(mcmc_combined_sample)

      ESS_df <- t(as.data.frame(ESS))
      rownames(ESS_df) <- NULL
      allESS <- rbind(allESS, ESS_df)


    }
  } else (print("run1 different from run2: double check data!"))
}


mean(allESS[,3])
