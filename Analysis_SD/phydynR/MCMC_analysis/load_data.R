library(ape)
library(phydynR)
library(stringr)
library(migrations)
library(treedater)

#read treedater tree
tree <- readRDS("../../Results_Benefits_paper/region1000global500/model0/sim1/trees/HIValignments/treedater_10000bp/dated_tree.RDS")
#tree <- readRDS("dated_tree.RDS")




# organize metadata in 2 columns.
# the first column is the sequence names
# the second column is the state (I, T and src) of each sequences
# where I = infected individuals
# where T = infected on treatment
# src = source sequences to account for importation

all_metadata <- unlist(lapply(tree$tip.label, function(x) get_metadata(x)))


#create matrix to receive the information on states for each tip of the tree
I <- src <- rep(0, length(tree$tip.label))

I[all_metadata == "I"] <- 1
src[all_metadata == "src"] <- 1

sampleStates <- cbind(I, src)
rownames(sampleStates) <- tree$tip.label


# create an object of DatedTree [phydynR package]
# This is the tree that should be used in the calculation of the likelihood
# to estimate parameter values with phydynR
dated.tree_sim <- DatedTree(phylo = tree,
                            sampleTimes = tree$sts,
                            sampleStates = sampleStates,
                            minEdgeLength = 0.0001,
                            tol = 1e-6)


