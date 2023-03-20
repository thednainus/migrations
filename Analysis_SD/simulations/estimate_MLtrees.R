# Estimate ML trees using iqtree2

# iqtree2 was installed in the cluster using conda environment and
# the command: conda install -c bioconda iqtree

# Estimate phylogenetic tree using IQ-TREE
# this script takes as argument "1000" or "10000"
# to specify alignments of 1000bp or 10000bp
# It also takes as argument the number of CPUs to use to estimate the trees
# using iqtree program

# start of script
start_time <- Sys.time()

# percentage of population to sampled IDs
get_params <- commandArgs(trailingOnly = TRUE)

seqlength <- as.numeric(get_params[1])
seq_length <- paste(seqlength, "bp", sep = "")


#to run iqtree
Software <- "iqtree"
maxCPU <- as.numeric(get_params[2])


#make dir to save iqtree results
iqtree_dirname <- paste("iqtree_results_", seq_length, sep = "")
if (!dir.exists(iqtree_dirname)) {
  dir.create(iqtree_dirname, recursive = TRUE)
}

#alignment name
ali <- paste("model0_ali_", seq_length, ".fasta", sep = "" )

ali_name <- paste("-s", ali, sep = " ")
# -czb Collapse near zero branches, so that the final tree may be
# multifurcating.
iqtree_param <- c(ali_name, "-m HKY", "-T AUTO", "-ntmax", maxCPU, "-czb")
#run iqtree
system2(command = Software, args = iqtree_param)


#end of script
end_time <- Sys.time()
print("IQTREE simulation took:")
end_time - start_time

iqtree_time <- data.frame(start = start_time, end = end_time)
saveRDS(iqtree_time, paste(seq_length, "iqtree_time.RDS", sep = "_"))






