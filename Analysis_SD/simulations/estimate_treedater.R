library(treedater)
library(ape)
library(stringr)

# start of script
start_time <- Sys.time()

#arguments will be the sequence length and the name of the alignment file
get_params <- commandArgs(trailingOnly = TRUE)

seqlength <- as.numeric(get_params[1])
seq_length <- paste(seqlength, "bp", sep = "")
aliname <- as.character(get_params[2])

#estimate treedated tree
#read ML tree
mltree <- read.tree(aliname)

#check if tree has polytomies
#if tree has polytomies resolve polytomies randomly
if(is.binary(mltree) == FALSE){
  mltree <- multi2di(mltree)
  if(is.rooted(mltree) == TRUE){
    mltree <- unroot(mltree)
  }
}

#get sample times from the names of the phylogenetic tree tips
sampleTimes <- as.numeric(unlist(lapply(mltree$tip.label, function(x)
  tail(strsplit(x, "_")[[1]], n= 1))))
sampleTimes <- setNames(sampleTimes, mltree$tip.label)

#run treedater
dated_tree <- dater(tre = mltree, sts = sampleTimes[mltree$tip.label],
                    s = seqlength, clock = "uncorrelated")


#end of script
end_time <- Sys.time()
print("treedater simulation took:")
end_time - start_time

treedater_time <- data.frame(start = start_time, end = end_time)
saveRDS(treedater_time, paste(seq_length, "treedater_time.RDS", sep = "_"))

#dave dated tree
saveRDS(dated_tree, "dated_tree.RDS")

