#merge logs from beast1 analysis
library(stringr)

#Software: BEAST 1.10.4 must be installed in the computer
Software <- "/Applications/BEAST_1.10.4/bin/logcombiner"


# for the partial pol gene ----

# Obtain an example log file its name
filenames <- list.files(path = "~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast1/importation_rate/high/region100global100",
                        all.files = TRUE, pattern = "phylogeography_beast1.log",
                        full.names = TRUE, recursive = TRUE)

run1 <- filenames[grepl("run1/results/", filenames)]
run2 <- filenames[grepl("run2/results/", filenames)]
run3 <- filenames[grepl("run3/results/", filenames)]
run4 <- filenames[grepl("run4/results/", filenames)]
run5 <- filenames[grepl("run5/results/", filenames)]
run6 <- filenames[grepl("run6/results/", filenames)]

#merge logs

for(i in 1:50){

  texts1 <- str_split(run1[i], "/")
  texts2 <- str_split(run2[i], "/")
  texts3 <- str_split(run3[i], "/")
  texts4 <- str_split(run4[i], "/")
  texts5 <- str_split(run5[i], "/")
  texts6 <- str_split(run6[i], "/")


  #replicate number
  rep_run1 <- str_split(texts1[[1]][14], "_")[[1]][2]
  rep_run2 <- str_split(texts2[[1]][14], "_")[[1]][2]
  rep_run3 <- str_split(texts3[[1]][14], "_")[[1]][2]
  rep_run4 <- str_split(texts4[[1]][14], "_")[[1]][2]
  rep_run5 <- str_split(texts5[[1]][14], "_")[[1]][2]
  rep_run6 <- str_split(texts6[[1]][14], "_")[[1]][2]

  #make sure we are combining logs from same sequence alignments
  ali_run1 <- texts1[[1]][13]
  ali_run2 <- texts2[[1]][13]
  ali_run3 <- texts3[[1]][13]
  ali_run4 <- texts4[[1]][13]
  ali_run5 <- texts5[[1]][13]
  ali_run6 <- texts6[[1]][13]

  output_location1 <- paste(texts1[[1]][1:14], collapse = "/")

  if((rep_run1 == rep_run2) &
     (rep_run1 == rep_run3) &
     (rep_run1 == rep_run4) &
     (rep_run1 == rep_run5) &
     (rep_run1 == rep_run6) &
     (ali_run1 == ali_run2) &
     (ali_run1 == ali_run3) &
     (ali_run1 == ali_run4) &
     (ali_run1 == ali_run5) &
     (ali_run1 == ali_run6)
     ){

    if(ali_run1 == "results"){
      #use logcombiner to combine logs from the same replicate
      output_name <- paste(output_location1, "combined_runs123456.log", sep = "/")
      args_run123 <- c("-burnin 20000000",
                        run1[i],
                        run2[i],
                        run3[i],
                        run4[i],
                        run5[i],
                        run6[i],
                        output_name)

      system2(Software, args_run123)
    }
  }
}




# for the complete HIV genome ----
# Obtain an example log file its name
filenames <- list.files(path = "~/Desktop/Imperial/newHIVproject-01Aug2020/BEAST_analysis/beast1/importation_rate/medium/region100global100",
                        all.files = TRUE, pattern = "phylogeography_beast1.log",
                        full.names = TRUE, recursive = TRUE)

run1 <- filenames[grepl("run1/results_10000bp/", filenames)]
run2 <- filenames[grepl("run2/results_10000bp/", filenames)]
run3 <- filenames[grepl("run3/results_10000bp/", filenames)]
run4 <- filenames[grepl("run4/results_10000bp/", filenames)]


#merge logs

for(i in 1:50){

  texts1 <- str_split(run1[i], "/")
  texts2 <- str_split(run2[i], "/")
  texts3 <- str_split(run3[i], "/")
  texts4 <- str_split(run4[i], "/")



  #replicate number
  rep_run1 <- str_split(texts1[[1]][14], "_")[[1]][2]
  rep_run2 <- str_split(texts2[[1]][14], "_")[[1]][2]
  rep_run3 <- str_split(texts3[[1]][14], "_")[[1]][2]
  rep_run4 <- str_split(texts4[[1]][14], "_")[[1]][2]


  #make sure we are combining logs from same sequence alignments
  ali_run1 <- texts1[[1]][13]
  ali_run2 <- texts2[[1]][13]
  ali_run3 <- texts3[[1]][13]
  ali_run4 <- texts4[[1]][13]


  output_location1 <- paste(texts1[[1]][1:14], collapse = "/")

  if((rep_run1 == rep_run2) &
     (rep_run1 == rep_run3) &
     (rep_run1 == rep_run4) &
     (ali_run1 == ali_run2) &
     (ali_run1 == ali_run3) &
     (ali_run1 == ali_run4)
  ){

    if(ali_run1 == "results_10000bp"){
      #use logcombiner to combine logs from the same replicate
      output_name <- paste(output_location1, "combined_runs24.log", sep = "/")
      args_run123 <- c("-burnin 40000000",

                       run2[i],

                       run4[i],
                       output_name)

      system2(Software, args_run123)
    }
  }
}
