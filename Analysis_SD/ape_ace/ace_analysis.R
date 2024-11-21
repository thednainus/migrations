library(ape)
library(treedater)
library(stringr)
library(dplyr)

trees <- list.files("Analysis_SD/phydynR/MCMC_results/ICL_cluster/importation_rate/low/region1000global1000/all_tree/run1", pattern = "dated_tree.RDS", recursive = TRUE,
                    full.names = TRUE)

results_all <- tibble()

for(i in 1:length(trees)){

  texts <- str_split(trees[i], "/")
  #simulation number
  # I end up analyzing 50 replicates. I removed the other replicates
  sim_number <- as.numeric(str_split(texts[[1]][11], "_")[[1]][2])

  if(sim_number <= 50){

    tips <- texts[[1]][7]
    mig_rate <- texts[[1]][6]
    seqlen <- texts[[1]][10]

    if(mig_rate == "medium"){
      mig_rate <-  0.1
    }else if(mig_rate == "low"){
      mig_rate <-  0.03
    }else if(mig_rate == "high"){
      mig_rate <-  0.33
    }

    if(seqlen == "results"){

      seqlen <- "1000bp"
    } else{

      seqlen <- "10000bp"
    }


    datedTree <- readRDS(trees[i])

    x <- as.factor(unlist(lapply(datedTree$tip.label, function(x) str_split(x, "_")[[1]][2])))
    anc <- ace(x, datedTree, type = "discrete", model ="ARD")

    results <- tibble(sim_number = sim_number,
                      tips = tips,
                      seqlen = seqlen,
                      mig_rate = mig_rate,
                      loglik = anc$loglik,
                      location = c(anc$index.matrix[2,1], anc$index.matrix[1,2]),
                      rates = anc$rates,
                      se = anc$se)

    results_all <- rbind(results_all, results)

  }
}

filename <- paste(texts[[1]][6], texts[[1]][7], "ace", sep = "_")
filename <- paste(filename, ".RDS", sep = "")

saveRDS(results_all, filename)



