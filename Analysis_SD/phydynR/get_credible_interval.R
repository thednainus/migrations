#reminder on how to use coda to check MCMC samples
library(coda)
library(BayesianTools)
library(dplyr)
library(stringr)
library(ggplot2)

#get the data that was completed i = 800 iterations

#list the name of files with results
#run1
mcmc_results1 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/low/region1000global500/mh1990/run1/results_10000bp", recursive = TRUE, full.names = TRUE)
#run2
mcmc_results2 <- list.files("Analysis_SD/phydynR/mcmc_results/ICL_cluster/importation_rate/low/region1000global500/mh1990/run2/results_10000bp", recursive = TRUE, full.names = TRUE)

#run1
mcmc_results_iter.data1 <- mcmc_results1[grepl("iter.rdata", mcmc_results1)]
mcmc_results_iter.data1 <- mcmc_results_iter.data1[-c(8,25,31,38)]
length(mcmc_results_iter.data1)
mcmc_results_out1 <- mcmc_results1[grepl("out_sim.RDS", mcmc_results1)]
mcmc_results_out1 <- mcmc_results_out1[-c(8,25,31,38)]
length(mcmc_results_out1)

#run2
mcmc_results_iter.data2 <- mcmc_results2[grepl("iter.rdata", mcmc_results2)]
mcmc_results_iter.data2 <- mcmc_results_iter.data2[-c(8,25,31,38)]
length(mcmc_results_iter.data2)

mcmc_results_out2 <- mcmc_results2[grepl("out_sim.RDS", mcmc_results2)]
mcmc_results_out2 <- mcmc_results_out2[-c(8,25,31,38)]
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
    out1_coda <- BayesianTools::getSample(out1, start = 5000, coda = TRUE)
    out2_coda <- BayesianTools::getSample(out2, start = 5000, coda = TRUE)
    #quartz()
    #plot(out_coda, ask = TRUE)

    #for run1
    summary_results1 <- summary(out1_coda)
    lower1 <- summary_results1$quantiles[3,1]
    median1 <- summary_results1$quantiles[3,3]
    upper1 <- summary_results1$quantiles[3,5]

    #for run2
    summary_results2 <- summary(out2_coda)
    lower2 <- summary_results2$quantiles[3,1]
    median2 <- summary_results2$quantiles[3,3]
    upper2 <- summary_results2$quantiles[3,5]

    quantiles_results <- tibble(index = j,
                                tips = tips,
                                seqlen = seqlen,
                                replicate = rep,
                                mig_rate = mig_rate,
                                tree_data = tree_data,
                                lower1 = lower1,
                                median1 = median1,
                                upper1 = upper1,
                                lower2 = lower2,
                                median2 = median2,
                                upper2 = upper2)

    CI_all <- rbind(CI_all, quantiles_results)
  }
}

View(CI_all)

filename <- paste(CI_all$tips[1], CI_all$tree_data[1], CI_all$mig_rate[1],
                  CI_all$seqlen[1], sep = "_")
print(filename)
filename <- paste(filename, ".RDS", sep = "")
saveRDS(CI_all, filename)






CI_all_sorted <- CI_all[order(CI_all$median1),]
CI_all_sorted["order_rep"] <- 1:nrow(CI_all_sorted)
CI_all_sorted$order_rep <- as.factor(CI_all_sorted$order_rep)


prior <- rlnorm(10000,log(1/20), 1/4)
prior_mig <- quantile(prior, probs = c(0.25, 0.5, 0.975))

prior_mig_df <- data.frame(lower = prior_mig[1],
                           median = prior_mig[2],
                           upper = prior_mig[3],
                           order_rep = 50)
prior_mig_df$order_rep <- as.factor(prior_mig_df$order_rep)

quartz()


ggplot(CI_all_sorted, aes(x = order_rep, y = median1)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upper1, ymin = lower1)) +
  geom_hline(yintercept=0.33, linetype="dashed") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  ggtitle("Credible interval for posterior: 1,000 region tips and 100 global tips: 1,000bp (all_tree)") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))


quartz()

ggplot(CI_all_sorted, aes(x = order_rep, y = median)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_point(data = prior_mig_df, aes(x = order_rep, y = median), size = 2) +
  geom_errorbar(data = prior_mig_df, ymax = upper, ymin = lower) +
  geom_hline(yintercept=0.1, linetype="dashed") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  ggtitle("Credible interval for posterior: 1,000 region tips and 500 global tips: 9,719bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))



CI_all$replicate <- as.factor(CI_all$replicate)

quartz()
ggplot(CI_all, aes(x = replicate, y = median)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upper, ymin = lower)) +
  geom_hline(yintercept=0.1, linetype="dashed") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bw() +
  ggtitle("Credible interval for posterior: 1,000 region tips and 500 global tips: 9,719bp") +
  ylab("Median and 2.5% and 97.5% quantiles") +
  xlab("Replicate number") +
  theme(text = element_text(size=14))

