#reminder on how to use coda to check MCMC samples
library(coda)
library(BayesianTools)

#get the data that was completed i = 800 iterations

#list the name of files with results
mcmc_results <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/high/region1000global500/mh1990/results_10000bp", recursive = TRUE, full.names = TRUE)
#mcmc_results <- list.files("Analysis_SD/phydynR/mcmc_results/QMUL_cluster/region1000global100/results_10000bp", recursive = TRUE, full.names = TRUE)

mcmc_results_iter.data <- mcmc_results[grepl("iter.rdata", mcmc_results)]
mcmc_results_out <- mcmc_results[grepl("out_sim.RDS", mcmc_results)]

for(j in 1:length(mcmc_results_iter.data)){

  load(mcmc_results_iter.data[j])
  print(i)

  if(i == 401){

    print(j)

    out <- readRDS(mcmc_results_out[j])
    #out1 <- getSample(out, start = 7500)
    #summary(out1)

    #convert to coda
    out_coda <- BayesianTools::getSample(out, start = 3000,coda = TRUE)
    #quartz()
    plot(out_coda, ask = TRUE)





    #teste <- summary(out_coda)
    #marginalPlot(out, names = c(ESTNAMES, ELOWER, EUPPER), start = 7500)


  }




}








out <- readRDS("Analysis_SD/phydynR/mcmc_results/region1000global500/1000bp/stage6_1/out_sim.RDS")

#convert to coda
out_coda <- BayesianTools::getSample(out, start = 1, coda = TRUE)
summary(out_coda)
#quartz()

#plot results
plot(out_coda, ask = TRUE)
