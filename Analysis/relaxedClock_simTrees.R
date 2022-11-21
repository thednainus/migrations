# Script that will get a phylogenetic tree and convert it to a
# log-normal relaxed clock tree

#Patino-Galindo and Gonzalez-Candelas 2017, Virus Evolution 3(2): vex029
# clock rate for subtype C-1
# all genes = 3.5*10e-3 (2.9*10e-3, 4.1*10e-3)
# pol gene = 1.7*10e-3 (1.5*10e-3, 1.9*10e-3)

library(simclock)

# read phylogenetic trees
setwd("~/Desktop/Imperial/newHIVproject-01Aug2020/R_projects/hivWsim/OSG/test_data/")
r_files <- list.files(pattern = "*.RData")

convert2relaxed_trees <- function(file_name, r0, s2){
  load(file_name)

  #remove .RData from filename
  newfilename <- unlist(strsplit(file_name, "[.]"))[1]
  newfilename <- paste(newfilename, "relx", sep = "_")
  newfilename <- paste(newfilename, "rds", sep = ".")


  relx_tree <- relaxed.tree(bdt, model="iln", r= r0, s2 = 1)

  saveRDS(object = relx_tree, file = newfilename)

}


convert2relaxed_trees(r_files[1], r0 = 0.035, s2 = 1)


lapply(r_files, FUN = convert2relaxed_trees, r0 = 0.035, s2 = 1)


