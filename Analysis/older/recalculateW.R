# calculate W on Stephane data to make sure the function is or not different
# this was done incorrectly, and I removed the lines that was not correct to avoid
# misunderstanding in the future.
# I kept this script because it might be useful in the future.

setwd("~/Desktop/Imperial/newHIVproject-01Aug2020/data_LeVupaper/simulations/model0-simulateBaseline0/")


calculateW <- function(filename){

  load(filename)
  # rescale tree
  tree <- daytree
  sampleTimes <- days2years(tree$sampleTimes )
  tree$edge.length <- tree$edge.length / 365
  bdt <- DatedTree(tree, sampleTimes, tree$sampleStates, tol = Inf)

  n<- bdt$n

  treeSampleStates <- tree$sampleStates
  cd4s <- setNames( sapply( 1:nrow(treeSampleStates), function(k){
    deme <- DEMES[ which.max(treeSampleStates[k,] )  ]
    stage <- strsplit( deme, '.' , fixed=T)[[1]][1]
    stage <- as.numeric( tail( strsplit(stage, '')[[1]], 1 ) )
    if (stage==1) return(1e3)
    if (stage==2) return(750)
    if (stage==3) return(400)
    if (stage==4) return(300)
    if (stage==5) return(100)
  }), tree$tip.label)
  ehis <-  setNames( sapply( 1:nrow( treeSampleStates), function(k){
    deme <- DEMES[ which.max(treeSampleStates[k,] )  ]
    stage <- strsplit( deme, '.' , fixed=T)[[1]][1]
    stage <- as.numeric( tail( strsplit(stage, '')[[1]], 1 ) )
    ifelse( stage==1, TRUE, FALSE)
  }), tree$tip.label)
  sampleDemes <- setNames( sapply( 1:n, function(u) DEMES[which.max( treeSampleStates[u,])] ), tree$tip.label )


  W <- phylo.source.attribution.hiv.msm( bdt
                                         , bdt$sampleTimes # must use years
                                         , cd4s = cd4s[bdt$tip.label] # named numeric vector, cd4 at time of sampling
                                         , ehi = ehis[bdt$tip.label] # named logical vector, may be NA, TRUE if patient sampled with early HIV infection (6 mos )
                                         , numberPeopleLivingWithHIV  = plwhiv# scalar
                                         , numberNewInfectionsPerYear = newinf # scalar
                                         , maxHeight = MH
                                         , res = 1e3
                                         , treeErrorTol = Inf)

  taxa_size <- Ntip(bdt)

  results <- c(taxa_size, IP80, IP90)

}

calculateW(r_files[1])
r_files <- list.files(pattern = "*.RData")

Wresults = lapply(r_files, calculateW)
saveRDS(Wresults, "~/Desktop/Imperial/newHIVproject-01Aug2020/R_projects/hivWsim/newW_LeVu_Baseline.rds")







# proportion of sampled at 2012
Le Vu data
data2012 <- subset(bdt$sampleTimes, bdt$sampleTimes >= 2012 & bdt$sampleTimes < 2013)
length(data2012)/ length(bdt$sampleTimes) *100

length(bdt$sampleTimes)/length(data2012)

5.475173

# my data
data2012 <- subset(bdt$sampleTimes, bdt$sampleTimes >= 2012 & bdt$sampleTimes < 2013)
length(data2012)/ length(bdt$sampleTimes) *100


data2012 <- subset(timeyears, timeyears >= 2012 & timeyears < 2013)
length(data2012)/ length(timeyears) *100

