library(ape)

###### PART 2: Subtype genetic sequences using COMET  --------------
# Analysing results from using COMET HIV-1 (https://comet.lih.lu/) for
# subtyping sequences.
################################################################################

SD_st <- read.delim("Analyses/Subtyping/SDiego_COMET.csv")
SD_st$subtype <- as.factor(SD_st$subtype)

#check which subtype is the most frequent one
plot(SD_st$subtype) # subtype B is the most frequent one


# subset dataframes and check only sequences that are not from subtype B
# these sequences will be later removed from alignments
sSD_st <- subset(SD_st, subtype != "B")


################################################################################
# Remove sequences that are not from subtype B from the alignment

# Read sequences
SD_seq <- read.FASTA("Analyses/Sequences/SD_all_data.fasta")


# get only sequences classified as subtype B
SD_stB <- SD_seq[!names(SD_seq) %in% sSD_st$name]
write.FASTA(SD_stB, file = "SD_pol.fasta")
