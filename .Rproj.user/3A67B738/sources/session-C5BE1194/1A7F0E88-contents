# Script that will get a phylogenetic tree and convert it to a
# log-normal relaxed clock tree

# It will also get the new simulated tree and simulate a DNA sequence
# alignment using this new phylogenetic tree

#Patino-Galindo and Gonzalez-Candelas 2017, Virus Evolution 3(2): vex029
# clock rate for subtype C-1
# all genes = 3.5*10^-3 (2.9*10^-3, 4.1*10^-3)
# pol gene = 1.7*10^-3 (1.5*10^-3, 1.9*10^-3)

library(simclock)
library(ape)
library(phyclust)

# read phylogenetic trees
setwd("~/Desktop/Imperial/newHIVproject-01Aug2020/R_projects/hivWsim/OSG/test_data/")
r_files <- list.files(pattern = "*.RData")

# convert phylogenetic trees to relaxed clock trees
#convert2relaxed_trees(r_files[1], r0 = 0.035, s2 = 1)

lapply(r_files, FUN = convert2relaxed_trees, r0 = 0.035, s2 = 1, par_values = "-mHKY -l100")

# This part of the script will read the new phylogenetic relaxed clock trees
# and simulate a DNA sequence alignment.
relx_files <- list.files(pattern = "*.rds")

lapply(relx_files, FUN = simulate_dna, par_values = "-mHKY -l100")





convert2relaxed_trees2 <- function(file_name, r0, s2, par_values){
  load(file_name)

  #remove .RData from filename
  newfilename <- unlist(strsplit(file_name, "[.]"))[1]
  treefilename <- paste(newfilename, "relx", sep = "_")
  treefilename <- paste(newfilename, "rds", sep = ".")

  # convert ultrametric tree to relaxed clock tree
  relx_tree <- relaxed.tree(bdt, model="iln", r= r0, s2 = s2)
  saveRDS(object = relx_tree, file = treefilename)

  # simulate DNA sequence alignment based on the relaxed clock trees
  dnafilename <- paste(newfilename, "dna", sep = "_")


  DNAali <- simulate_dna(tree = relx_tree, par_values = par_values)
  write_ali(seqgen_ali = DNAali, filename = dnafilename)



}


