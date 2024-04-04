#estimate the Gelman statistics to access convergence of the independent runs
library(coda)
library(BayesianTools)
library(stringr)

#get the data that was completed i = 800 iterations

#list the name of files with results
mcmc_results1 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/low/region1000global500/all_tree/run1/results", recursive = TRUE, full.names = TRUE)
mcmc_results2 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/low/region1000global500/all_tree/run2/results", recursive = TRUE, full.names = TRUE)

mcmc_results_iter.data1 <- mcmc_results1[grepl("iter.rdata", mcmc_results1)]
length(mcmc_results_iter.data1)
mcmc_results_out1 <- mcmc_results1[grepl("out_sim.RDS", mcmc_results1)]
length(mcmc_results_out1)

mcmc_results_iter.data2 <- mcmc_results2[grepl("iter.rdata", mcmc_results2)]
length(mcmc_results_iter.data2)
mcmc_results_out2 <- mcmc_results2[grepl("out_sim.RDS", mcmc_results2)]
length(mcmc_results_out2)

for(j in 1:length(mcmc_results_iter.data1)){

  run_number1 <- str_split(mcmc_results_iter.data1[j], "/")[[1]][11]
  run_number2 <- str_split(mcmc_results_iter.data2[j], "/")[[1]][11]

  if(run_number1 != run_number2){

    run_number1 <- str_split(run_number1, "_")[[1]][2]
    run_number2 <- str_split(run_number2, "_")[[1]][2]


  }

  if(run_number1 == run_number2){

    load(mcmc_results_iter.data1[j])
    #print(i)
    i1 <- i

    load(mcmc_results_iter.data2[j])
    #print(i)
    i2 <- i

    #then it will
    if(i1 == i2 & i1 == 801){


      #print(j)
      #print(mcmc_results_iter.data1[j])
      #print(mcmc_results_iter.data2[j])


      out1 <- readRDS(mcmc_results_out1[j])
      out2 <- readRDS(mcmc_results_out2[j])

      #convert to coda
      out_sample1 <- BayesianTools::getSample(out1, start=5000, coda = TRUE)
      out_sample2 <- BayesianTools::getSample(out2, start=5000, coda = TRUE)

      mcmc_combined <- createMcmcSamplerList(list(out1, out2))


      #quartz()
      #as a rule of thumb values below 1.1 or so is OK
      #(https://theoreticalecology.wordpress.com/2011/12/09/mcmc-chain-analysis-and-convergence-diagnostics-with-coda-in-r/)
      convergence_res <- gelmanDiagnostics(mcmc_combined, plot = FALSE)
      #print(convergence_res)

      #if(convergence_res$mpsrf >= 1.10){

        #print(j)

      #}

      if(convergence_res$psrf[3,2] >= 1.10){

        print(j)

      }




      #teste <- summary(out_coda)
      #marginalPlot(out, names = c(ESTNAMES, ELOWER, EUPPER), start = 7500)

      #get ESS values
      #print(ESS1 <- coda::effectiveSize(out_sample1))
      #print(ESS2 <- coda::effectiveSize(out_sample2))

    }
  } else (print("run1 different from run2: double check data!"))
}








out <- readRDS("Analysis_SD/phydynR/mcmc_results/region1000global500/1000bp/stage6_1/out_sim.RDS")

#convert to coda
out_coda <- BayesianTools::getSample(out, start = 1, coda = TRUE)
summary(out_coda)
#quartz()

#plot results
plot(out_coda, ask = TRUE)
