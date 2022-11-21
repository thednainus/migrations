# Comparisons of my and Stephane simulated data
# to make sure I am using the right scripts
# the IP80_perc or IP90_perc was incorrectly calculate
# I kept this script because the idea might be useful in the future

setwd("~/Desktop/Imperial/newHIVproject-01Aug2020/R_projects/hivWsim/")

#LeVuER <- readRDS("W_LevuEqualRates.rds")
#LeVuER["Model"] <- "LeVuER"
LeVuBL <- readRDS("W_LevuBaseline.rds")
LeVuBL["Model"] <- "1.LeVuBL"

newWLeVuBL <- readRDS("W_LeVu_Baseline.rds")
newWLeVuBL["Model"] <- "2.newW_LeVuBL"

Fab <- readRDS("W.rds")
Fab12164 <- subset(Fab, taxa_size == 12164)
Fab12164["Model"] <- "3.Fab 12164"

#Fab_dock <- readRDS("Fab_dockModel.rds")
#Fab_dock["Model"] <- "Fab dock 12164"


all_data <- rbind(LeVuBL, newWLeVuBL, Fab12164)


mdf <- melt(all_data[c("taxa_size", "IP80_perc", "IP90_perc", "Model")], id=c("taxa_size", "Model"))

ggplot(mdf, aes(x=Model, y=value, fill = variable)) + geom_boxplot() +
  theme_bw() + ylab("percentage")
