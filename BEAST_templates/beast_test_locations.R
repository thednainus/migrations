library(ape)
library(phangorn)
library(seqinr)
library(stringr)

tree_teste <- read.alignment("../Benefits/test_results/model0/sim1/trees/alignments/model0_ali_1000bp.fasta", format = "FASTA")

names_tip <- tree_teste$nam

locations <- unlist(lapply(names_tip, function(x) strsplit(x, "_")[[1]][1]))

locations <- unlist(lapply(locations, function(x) ifelse(x == "simsrc", "src" , "region")))

data_beast <- data.frame(tips = names_tip, location = locations)

write.table(data_beast, file = "teste_beast_states.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
