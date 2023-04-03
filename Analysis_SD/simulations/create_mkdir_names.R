#create file names

ephemeral_dir <- "/rds/general/user/fferre15/ephemeral/SD_Benefits/model0"

n_jobs <- 100

dirnames <- NULL

for (i in 1:n_jobs){

  sim_number <- paste("sim", i, "/trees", sep = "")
  namedir <- paste(ephemeral_dir, sim_number, sep = "/")

  dirnames <- c(dirnames, namedir)

}

write.table(dirnames, "mkdir_names.txt", append = FALSE, quote = FALSE,
            sep = " ", dec = ".", row.names = FALSE,
            col.names = FALSE)
