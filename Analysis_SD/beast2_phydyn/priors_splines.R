# defining upper and lower values for priors
#ADD THE UPPER and LOWER bound too



beta1_f <- function(t, b1){

  if (t<1980){
    beta1 <- 0.0
  } else {
    beta1 <- (max(0.0, b1))
  }

  return(beta1)

}


betalb_f <- function(t, beta1, dlogbeta){

  if ( t <= 1980 ){
    betalb <- 0
  } else if (t  <= 1985){
    betalb <- beta1
  } else if (t  <= 1990 ) {
    betalb <- beta1 * exp( dlogbeta[1] )
  }else if (t  <= 1995 ){
    betalb <- beta1 * exp( dlogbeta[1] + dlogbeta[2] )
  } else if (t <= 2000 ){
    betalb <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] )
  } else if (t <= 2005 ){
    betalb <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] + dlogbeta[4] )
  } else if (t <= 2010 ){
    betalb <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] + dlogbeta[4] + dlogbeta[5] )
  } else if (t <= 2015 ){
    betalb <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] + dlogbeta[4] + dlogbeta[5] + dlogbeta[6] )
  } else {
    betalb <- (beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] + dlogbeta[4] + dlogbeta[5] + dlogbeta[6] + dlogbeta[7] ) )
  }

  return(betalb)

}

betaub_f <- function(t, beta1, dlogbeta){

  if ( t <= 1980 ){
    betaub <- 0
  } else if (t  <= 1985 ){
    betaub <- beta1 * exp( dlogbeta[1] )
  } else if (t  <= 1990 ){
    betaub <- beta1 * exp( dlogbeta[1] + dlogbeta[2] )
  } else if (t  <= 1995 ){
    betaub <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] )
  } else if (t  <= 2000 ){
    betaub <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] + dlogbeta[4] )
  } else if (t  <= 2005 ){
    betaub <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] + dlogbeta[4] + dlogbeta[5] )
  } else if (t  <= 2010 ){
    betaub <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] + dlogbeta[4] + dlogbeta[5] + dlogbeta[6] )
  } else if (t  <= 2015 ){
    betaub <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] + dlogbeta[4] + dlogbeta[5] + dlogbeta[6] + dlogbeta[7] )
  } else {
    betaub <- beta1 * exp( dlogbeta[1] + dlogbeta[2] + dlogbeta[3] + dlogbeta[4] + dlogbeta[5] + dlogbeta[6] + dlogbeta[7] )

  }

  return(betaub)

}

tlb_f <- function(t){

  if (t <= 1980 ){
    tlb <- 1980
  } else if (t <= 1990 ){
    tlb <- 1985
  } else if (t <= 1995 ){
    tlb <- 1990
  } else if (t <= 2000 ){
    tlb <- 1995
  }else if (t <= 2005 ){
    tlb <- 2000
  }else if (t <= 2010 ){
    tlb <- 2005
  }else if (t <= 2015 ){
    tlb <- 2010
  } else if (t <= 2020){
    tlb <- 2015
  }else{
    tlb <- 2020
  }
  return(tlb)
}




total_splines <- function(t){

  #dlogbeta <- rnorm(7, 0, 0.5)
  dlogbeta <- rnorm(7, 0, 0.05)
  #b1 <- rnorm(1, 0, 0.6)
  b1 <- rnorm(1, 0, 2)
  #b1 <- rlnorm(1, 2.7, 0.5)

  beta1 <- beta1_f(t,b1)
  betalb <- betalb_f(t, beta1, dlogbeta)
  betaub <- betaub_f(t, beta1, dlogbeta)
  tlb <- tlb_f(t)

  propub <- max(0, min(1, (t-tlb)/(5)))
  beta <- max(0, propub * betaub + (1-propub)*betalb)


  all_data <- data.frame(time = t, beta = beta, b1 = b1, propub = propub, tlb = tlb, betalb = betalb, betaub = betaub, dlogbeta = paste(dlogbeta, collapse = "_"))

  return(all_data)
}

years <- seq(1980, 2020, by = 0.5)
all_data <- data.frame()
for(i in 1:100){

  for(t in years){
    betas <- total_splines(t)
    betas["i"] <- i
    all_data <- rbind(all_data, betas)
  }

}
