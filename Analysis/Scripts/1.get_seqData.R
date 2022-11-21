library(phylotools)
library(stringr)

###### PART 1: get genetic sequences  --------------

# Read csv file with San Diego data
sdiego_data <- read.csv("~/Desktop/Imperial/newHIVproject-01Aug2020/data_SDiego/OneDrive_1_11-26-2020/df_sequence_1452_nov252020.csv",
                        na.strings=c("","NA"))


# count number of base pairs and exclude gaps
sdiego_data["seq_length"] <- nchar(sdiego_data$sequence) - str_count(sdiego_data$sequence, "-")


# Remove duplicated sequences using pid participant_id
# I used the solution as described here
# https://stackoverflow.com/questions/25962909/remove-duplicates-based-on-2nd-column-condition
seq_dups <- sdiego_data[with(sdiego_data, ave(seq_length, pid, FUN = max) == seq_length),]
# However, seq_dups still contains some duplicates in case that consensus_length
# was identical between the duplicates
# to remove the rest of the duplicates, I do the below
noDups <- seq_dups[!duplicated(seq_dups$pid),]



# save data as FASTA format
SD_all_data <- noDups[c("pid", "sequence")]
colnames(SD_all_data) <- c("seq.name", "seq.text")
dat2fasta(SD_all_data, outfile = "SD_all_data.fasta")
