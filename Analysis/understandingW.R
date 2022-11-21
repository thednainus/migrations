library(ggplot2)

test <- load("11107_4805_1000.RData")
test2 <- load("10013_8222_7000.RData")
test.df <- as.data.frame(W)
test2.df <- as.data.frame(W)


W["donor"] %in% W["recip"]

W[which(W["donor"] %in% W["recip"])]

W90 <- subset(test.df, infectorProbability > 0.9)
W80 <- subset(test.df, infectorProbability > 0.8)


testeW <- W90[which(W90$donor %in% W90$recip),]








W90_2 <- subset(test2.df, infectorProbability > 0.9)
W80_2 <- subset(test2.df, infectorProbability > 0.8)


testeW <- W90[which(W90$donor %in% W90$recip),]
