# Simulate sequence alignments
# Seq-Gen MUST be installed in your computer

#start of script
start_time <- Sys.time()

library(stringr)
library(ape)

# Location for Seq-Gen. It should be changed to the correct location on your computer.
Software <- "/Applications/Seq-Gen-1.3.4/source/seq-gen"
#Software <- "Seq-Gen-1.3.4/source/seq-gen"
#parameter for Seq-Gen
#seq-gen -mHKY -t0.5 -fe -l10000 -n1 < filename > filename+'.txt'
# values for generating sequence alignment for HIV pol as described in van der Kyl and Berkhout, 2012
# values of -s0.0028 based on paper by Patino-Galindo and Gonzalez-Candelas 2017
# values of -t8.75 based on Chen et al 2004
# Updated parameters values based on sequences for San Diego + background sequences


# read tree in which branch lengths have been converted into years
load("/Users/sofia/Desktop/Imperial/R_packages/Benefits/test_results/model0/sim1/trees/trees.rda")
# convert tree topology into newick tree format
write.tree(phy = tree, file = "tree_year.tree")

#Create directory for sequence alignment
if (!dir.exists("alignments")) {
  dir.create("alignments")
  }



# simulate sequence alignment of 1000bp-
  seq_filename1000 <- paste("alignments/", filename_prefix, "_ali_1000bp.fasta", sep = "")
  seq_filename10000 <- paste("alignments/", filename_prefix, "_ali_10000bp.fasta", sep = "")


  # simulate sequence alignment using Seq-Gen
  #Simulate alignments of 1000 bp
  args1 <- c("-mHKY", "-l1000", "-t8.75", "-f0.389,0.165,0.228,0.218", "-s0.0028", "-n1", "-of", "tree_year.tree")
  system2(command = Software, args = args1, stdout = seq_filename1000)
  #Simulate alignments of 10000 bp
  args2 <- c("-mHKY", "-l10000", "-t8.75", "-f0.389,0.165,0.228,0.218", "-s0.0028", "-n1", "-of", "tree_year.tree")
  system2(command = Software, args = args2, stdout = seq_filename10000)


#end of script
end_time <- Sys.time()
print("Sequence alignment simulation took:")
end_time - start_time

seq_alignment_time <- data.frame(start = start_time, end = end_time)
saveRDS(seq_alignment_time, "seq_alignment_time.RDS")
