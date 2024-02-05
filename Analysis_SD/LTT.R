#Checking LTT (lineages through time for the treedater files)
library(treedater)

datedTree <- list.files(pattern = "*.RDS")

(param_boot <- parboot( datedTree, ncpu = 5))

saveRDS(param_boot, "param_boot.RDS")


# TO DO: 18/10/2023
#check for outliers and LTT for all replicates
#then check for distribution of sampled dates for region and global in separate for each replicate
#use parallel for BAyeasin Tools (I think I did that for some scripts for HIV in Senegal?)
#create template for BEAST
