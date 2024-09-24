library(ape)
library(phydynR)
library(BayesianTools)
library(SDBenefits)


#load Dated.tree object
#source("Analysis_SD/phydynR/load_data.R")
source("load_data.R")
#source("Analysis_SD/phydynR/model1.2_phydynR.R")
source("model1.2_phydynR.R")


#importRate=srcMigrationRate
#alpha = rate of ART initiation

ESTNAMES <- c('src1980', 'I1980', 'srcMigrationRate', 'alpha', betaNames)
ELOWER <-  c(1.00, 0.01, 1/150, 1/50, rep(0.001, length(betaNames) ) )
EUPPER <-  c(1e5, 500,   1/2, 1/1., rep(2, length(betaNames) ) )

# Specify a density function to be used in the prior specification (see below)
priordensity <- function(theta){
  names(theta) <- ESTNAMES
  o = dlnorm(theta['src1980'], log(1.5e3), 1/2 , log = TRUE ) +
      dexp(theta['I1980'], 1/5 , log = TRUE ) +
      dlnorm(theta['srcMigrationRate'], log(1/20), 1 , log = TRUE ) +
      sum( dlnorm(theta[betaNames], log(2/12), 1 , log = TRUE )  )
  unname( o )
}

# Create sampling, this is optional but recommended because the MCMCs can
# generate automatic starting conditions if this is provided
sampler <- function(n = 1){

  src1980 <- rlnorm(n, log(1.5e3), 1/2)
  I1980 <- rexp (n, 1/5 )
  srcMigrationRate <- rlnorm(n , log( 1/20 ), 1 )
  alpha <- runif(n, 1/8, 0.60)
  o = cbind(src1980, I1980 , srcMigrationRate, alpha,
            matrix( rlnorm( n*length(betaNames), log(2/12), 1/4), nrow = n) )
  for (i in 1:n){
    o[i,] <- pmax( ELOWER, o[i, ] )
    o[i, ] <- pmin( EUPPER, o[i,] )
  }
  return(o)
}

# Create prior (necessary for the BayesianTools package)
prior <- createPrior(density = priordensity,
                     sampler = sampler,
                     lower = ELOWER ,
                     upper = EUPPER
)

co0 <- function(theta){


  names(theta) <- ESTNAMES
  print(theta)
  p <- parms
  p[names(theta)] <- theta
  x0 <- c(src = p$src1980, I = p$I1980 , Tr = 0)
  s <-  dm(p,
           x0,
           t0 = 1980,
           t1 = dated.tree_sim$maxSampleTime,
           res = 100)
  y1 <- tail( s[[5]], 1 )[1,]
  zprior <- dnorm( y1['Tr'] / (y1['Tr'] + y1['I'] ), 0.5, sd = 0.05, log = TRUE )
  #~ browser()
  o = suppressWarnings( colik( tree = dated.tree_sim,
                               theta = p,
                               demographic.process.model = dm,
                               x0 = x0,
                               t0 = 1980,
                               res = 100,
                               maxHeight = dated.tree_sim$maxSampleTime - 1980,
                               likelihood = 'PL2') )
  print(o + zprior)
  o + zprior
}





#Implementing linear chain
load("iter.rdata")

while(i < 801){
  if(!file.exists("out_sim.RDS")){
    bayesianSetup3 <- createBayesianSetup(likelihood = co0 , prior = prior, names = ESTNAMES , parallel = 3)
    settings3 = list(iterations = 90, nrChains = 1, thin = 1)

    out <- runMCMC(bayesianSetup = bayesianSetup3, sampler = "DEzs", settings = settings3)
    saveRDS(out, "out_sim.RDS")
    i = i + 1
    save(i, file="iter.rdata")
    stopParallel(bayesianSetup3)
  }else{
    out <- readRDS("out_sim.RDS")
    out1 <- out
    bayesianSetup3 <- createBayesianSetup(likelihood = co0 , prior = prior, names = ESTNAMES , parallel = 3 )
    out <- runMCMC(bayesianSetup = out1)
    saveRDS(out, "out_sim.RDS")
    i = i + 1
    save(i, file="iter.rdata")
    stopParallel(bayesianSetup3)
  }
}

