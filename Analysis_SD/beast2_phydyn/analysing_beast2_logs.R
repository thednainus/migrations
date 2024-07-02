#merge logs from beast1 analysis
library(stringr)

#Software: BEAST 1.10.4 must be installed in the computer
Software <- "/Applications/BEAST_2.7.6/bin/logcombiner"

# Obtain an example log file its name
filenames <- list.files(path = "~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast2/importation_rate/medium/region100global100",
                        all.files = TRUE, pattern = "beast_phydyn_strictClock.log",
                        full.names = TRUE, recursive = TRUE)

run1 <- filenames[grepl("run1/results/", filenames)]
run1.numbers <- unlist(lapply(run1, function(x) str_split(x, "/")[[1]][14]))
run2 <- filenames[grepl("run2/results/", filenames)]
run2.numbers <- unlist(lapply(run2, function(x) str_split(x, "/")[[1]][14]))



#merge logs

for(i in 1:length(run1)){

  texts1 <- str_split(run1[i], "/")
  texts2 <- str_split(run2[i], "/")



  #replicate number
  rep_run1 <- str_split(texts1[[1]][14], "_")[[1]][2]
  rep_run2 <- str_split(texts2[[1]][14], "_")[[1]][2]


  #make sure we are combining logs from same sequence alignments
  ali_run1 <- texts1[[1]][13]
  ali_run2 <- texts2[[1]][13]


  output_location1 <- paste(texts1[[1]][1:14], collapse = "/")

  if((rep_run1 == rep_run2) & (ali_run1 == ali_run2)){

    if(ali_run1 == "results"){
      #use logcombiner to combine logs from the same replicate
      output_name <- paste(output_location1, "combined_runs12.log", sep = "/")
      args_run12 <- c(paste("-log", run1[i], sep = " "),
                      paste("-log", run2[i], sep = " "),
                      "-b 30",
                     paste("-o", output_name, sep = " "))

      system2(Software, args_run12)
    }




  }

}

