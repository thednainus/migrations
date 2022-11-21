setwd("~/Desktop/Imperial/newHIVproject-01Aug2020/R_projects/hivWsim/OSG/OSG_data/data")

library(phydynR)
library(ape)
library(reshape2)
library(ggplot2)


r_files <- list.files(pattern = "*.RData")

#load(r_files[6])



sumW <- function(file_name){
  load(file_name)
  taxa_size <- Ntip(bdt)

  # sum of the infector probabilities for a given phylogenetic tree
  sumW <- sum(W$infectorProbability)
  # count the number of pairs in which an infector probability was calculated
  countW <- length(W$infectorProbability)

  results <- c(taxa_size, sumW, countW)

}

Wsum_results <- lapply(r_files, sumW)
Wsumresults_df <- as.data.frame(t(as.data.frame(Wsum_results)))
rownames(Wsumresults_df) <- c(1:length(rownames(Wsumresults_df)))
colnames(Wsumresults_df) <- c("taxa_size", "sum", "count")



# select 100 simulations (some of the taxa_size has more than 100 simulated trees)
nWresults_df <- subset(Wsumresults_df, taxa_size < 6000 |
                         taxa_size == 7000)
Wresults5000 <- subset(Wsumresults_df, taxa_size == 5000)[1:100,]
Wresults6000 <- subset(Wsumresults_df, taxa_size == 6000)[1:100,]
Wresults7000 <- subset(Wsumresults_df, taxa_size == 7000)[1:100,]
Wresults8000 <- subset(Wsumresults_df, taxa_size == 8000)[1:100,]
Wresults9000 <- subset(Wsumresults_df, taxa_size == 9000)[1:100,]
Wresults10000 <- subset(Wsumresults_df, taxa_size == 10000)[1:100,]
Wresults11000 <- subset(Wsumresults_df, taxa_size == 11000)[1:100,]
Wresults12164 <- subset(Wsumresults_df, taxa_size == 12164)[1:100,]


allW <- rbind(nWresults_df, Wresults5000, Wresults6000, Wresults7000,
              Wresults8000, Wresults9000, Wresults10000, Wresults11000,
              Wresults12164)
allW$taxa_size <- as.factor(allW$taxa_size)








allWsum <- allW[,1:2]
meanW <- aggregate(allWsum[, 2], list(allWsum$taxa_size), mean)
colnames(meanW) <- c("taxa_size", "mean")


ggplot(meanW, aes(x=taxa_size, y=mean)) + geom_point() +
  theme_bw() + ylab("Expected number of transmission pairs") + xlab("Taxa size") +
  scale_colour_brewer(palette = "Set1") +
  theme(text = element_text(size = 20), legend.position="bottom")





