library(ape)
library(phangorn)
library(seqinr)
library(stringr)

tree_teste <- read.alignment("/Users/sofia/Desktop/Imperial/R_packages/SD_Benefits/Analysis_SD/simulations/alignments/teste_ali_1000bp.fasta", format = "FASTA")

names_tip <- tree_teste$nam

locations <- unlist(lapply(names_tip, function(x) strsplit(x, "_")[[1]][1]))

locations <- unlist(lapply(locations, function(x) ifelse(x == "simsrc", "src" , "region")))

data_beast <- data.frame(tips = names_tip, location = locations)

write.table(data_beast, file = "teste_beast_states.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
