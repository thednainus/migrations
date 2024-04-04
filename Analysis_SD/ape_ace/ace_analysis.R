library(ape)
library(treedater)
library(stringr)
library(dplyr)

trees <- list.files("/Users/sofia/Library/CloudStorage/OneDrive-ImperialCollegeLondon/HIV_SanDiego/results_papers/Results_Benefits_paper/importation_rate/medium/region1000global100/model0", pattern = "dated_tree.RDS", recursive = TRUE,
                    full.names = TRUE)

results_all <- tibble()

for(i in 1:length(trees)){

  texts <- str_split(trees[i], "/")
  #simulation number
  # I end up analysing 50 replicates. I removed the other replicates
  sim_number <- as.numeric(str_split(texts[[1]][14], "sim")[[1]][2])

  if(sim_number <= 50){

    tips <- texts[[1]][12]
    mig_rate <- texts[[1]][11]
    seqlen <- str_split(texts[[1]][17], "_")[[1]][2]

    if(mig_rate == "medium"){
      mig_rate <-  0.1
    }else if(mig_rate == "low"){
      mig_rate <-  0.33
    }else if(mig_rate == "high"){
      mig_rate <-  0.03
    }


    datedTree <- readRDS(trees[i])

    x <- as.factor(unlist(lapply(datedTree$tip.label, function(x) str_split(x, "_")[[1]][2])))
    anc <- ace(x, datedTree, type = "discrete", model ="ARD")

    results <- tibble(sim_number = sim_number,
                      tips = tips,
                      seqlen = seqlen,
                      mig_rate = mig_rate,
                      loglik = anc$loglik,
                      rates = anc$rates,
                      se = anc$se)

    results_all <- rbind(results_all, results)

  }
}

