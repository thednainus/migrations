# Simulate sequence alignments
# Seq-Gen MUST be installed in your computer

#start of script
start_time <- Sys.time()

library(stringr)
library(ape)

# Location for Seq-Gen. It should be changed to the correct location on your computer.
Software <- "/Applications/Seq-Gen-1.3.4/source/seq-gen"
#Software <- "Seq-Gen-1.3.4/source/seq-gen"
#reference genomic sequence to give as input to Seg-Gen
ref_genome <- "~/Desktop/Imperial/R_packages/SD_Benefits/Analysis_SD/input_files/K03455_partial_pol.phy"
#ref_genome <- "K03455_partial_pol.phy"
# file containing the number of trees in which to simulate alignments
number_trees <- "~/Desktop/Imperial/R_packages/SD_Benefits/Analysis_SD/input_files/tree_number.txt"
#number_trees <- "tree_number.txt"
#parameter for Seq-Gen
#seq-gen -mHKY -t0.5 -fe -l10000 -n1 < filename > filename+'.txt'
# values for generating sequence alignment for HIV pol as described in van der Kyl and Berkhout, 2012
# values of -s0.0028 based on paper by Patino-Galindo and Gonzalez-Candelas 2017
# values of -t8.75 based on Chen et al 2004
# Updated parameters values based on sequences for San Diego + background sequences


# read tree in which branch lengths have been converted into years
load("Analysis_SD/simulations/trees.rda")
# convert tree topology into newick tree format
write.tree(phy = tree, file = "Analysis_SD/simulations/tree_year.tree")

#Create directory for sequence alignment
if (!dir.exists("Analysis_SD/simulations/alignments_test")) {
  dir.create("Analysis_SD/simulations/alignments_test")
  }

filename_prefix <- "test_ancestral_seq"

#concatenate data for seqgen
system2(command = "cat",
        args = c(ref_genome, number_trees, "Analysis_SD/simulations/tree_year.tree"),
        stdout = "Analysis_SD/simulations/input_seqgen.txt")

# simulate sequence alignment of 1000bp-
  #seq_filename1000 <- paste("/Users/sofia/Desktop/Imperial/R_packages/SD_Benefits/Analysis_SD/simulations/alignments_test/", filename_prefix, "_ali_1000bp.fasta", sep = "")
  #seq_filename10000 <- paste("/Users/sofia/Desktop/Imperial/R_packages/SD_Benefits/Analysis_SD/simulations/alignments_test/", filename_prefix, "_ali_10000bp.fasta", sep = "")
  seq_filename1000 <- paste("/Users/user/Desktop/Imperial/newHIVproject/R_projects/SDBenefits/Analysis_SD/simulations/alignments/", filename_prefix, "_ali_1000bp.fasta", sep = "")
  seq_filename10000 <- paste("/Users/user/Desktop/Imperial/newHIVproject-01Aug2020/R_projects/SDBenefits/Analysis_SD/simulations/alignments/", filename_prefix, "_ali_10000bp.fasta", sep = "")

  # simulate sequence alignment using Seq-Gen
  #Simulate alignments of 1000 bp
  args1 <- c("-mHKY", "-t8.75", "-f0.389,0.165,0.228,0.218", "-s0.0028", "-n1", "-of", "-k1", "Analysis_SD/simulations/input_seqgen.txt")
  system2(command = Software, args = args1, stdout = seq_filename1000)
  #Simulate alignments of 10000 bp
  args2 <- c("-mHKY", "-t8.75", "-f0.389,0.165,0.228,0.218", "-s0.0028", "-n1", "-of", "Analysis_SD/simulations/tree_year.tree")
  system2(command = Software, args = args2, stdout = seq_filename10000)


#end of script
end_time <- Sys.time()
print("Sequence alignment simulation took:")
end_time - start_time

seq_alignment_time <- data.frame(start = start_time, end = end_time)
saveRDS(seq_alignment_time, "seq_alignment_time.RDS")
