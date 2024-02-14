#https://github.com/mrc-ide/sarscov2-phylodynamics/blob/739a5845a4ecee323b1822c9c1acc9ae3b0c01e0/r/sarscov2/R/xmlformatting.R#L77

#tstart = 2020.085
#tend = 2020.25
#numchange = 3

gen_gmrf_betablock(tend = 2020, tstart = 1980, numchange = 7)
gen_gmrf_betablock <- function(   tend, tstart = 2020.038, numchange = 3){
  #tstart = 2020.085
  #tend = 2020.25
  #numchange = 3
  library( glue )

  tknots = seq( tstart, tend, length.out = numchange+2 )
  ctknots <- as.list( as.character( tknots ))
  n <- length( ctknots )
  names(ctknots) <- paste0( 't', 1:n)

  betalb = '<definition id="Definition.betalb" spec="phydyn.model.Definition" value="betalb = if ( t !>= 2020.035 ) then 0'
  betalb = paste(sep='\n', betalb
                 , with( ctknots, glue( 'else if (t  !>= {t2} ) then beta1' ) )
  )
  for ( k in 3:(n-1)){
    dlogbetasum = paste(collapse=' + ', sapply( 0:(k-3), function(j) with ( list(y=as.character(j)), glue('dlogbeta[{y}]') ) ) )
    tindex = ctknots[ paste0( 't', k ) ]
    betalb = paste(sep='\n', betalb
                   , glue( 'else if (t  !>= {tindex} ) then beta1 * exp( {dlogbetasum} )' )
    )
  }
  dlogbetasum = paste(collapse=' + ', sapply( 0:(n-3), function(j) with ( list(y=as.character(j)), glue('dlogbeta[{y}]') ) ) )
  betalb = paste(sep='\n', betalb
                 , glue( 'else ( beta1 * exp( {dlogbetasum} ) )' )
                 , '"/>'
  )
  betalb = glue( betalb )

  betaub = '<definition id="Definition.betaub" spec="phydyn.model.Definition" value="betaub = if ( t !>= 2020.035 ) then 0'
  for ( k in 2:(n-1)){
    dlogbetasum = paste(collapse=' + ', sapply( 0:(k-2), function(j) with ( list(y=as.character(j)), glue('dlogbeta[{y}]') ) ) )
    tindex = ctknots[ paste0( 't', k ) ]
    betaub = paste(sep='\n', betaub
                   , glue( 'else if (t  !>= {tindex} ) then beta1 * exp( {dlogbetasum} )' )
    )
  }
  dlogbetasum = paste(collapse=' + ', sapply( 0:(n-3), function(j) with ( list(y=as.character(j)), glue('dlogbeta[{y}]') ) ) )
  betaub = paste(sep='\n', betaub
                 , glue( 'else ( beta1 * exp( {dlogbetasum} ) )' )
                 , '"/>'
  )
  betaub = glue( betaub )

  tlb =  with( ctknots, glue( '<definition id="Definition.tlb" spec="phydyn.model.Definition" value=" tlb  = if (t  !>=  2020.035 ) then {t1} ' ) )
  for ( k in 3:(n)){
    tindex0 = ctknots[ paste0( 't', k-1 ) ]
    tindex1 = ctknots[ paste0( 't', k ) ]
    tlb = paste(sep='\n', tlb
                , glue( 'else if (t  !>= {tindex1} ) then {tindex0}' )
    )
  }
  tindex = ctknots[ paste0( 't', n ) ]
  tlb = glue( paste(sep='\n', tlb,   'else({tindex}) "/>' ) )

  difftime = as.character( tknots[2] - tknots[1] )
  propub = '<definition id="Definition.propub" spec="phydyn.model.Definition" value=" propub = max(0, min(1, (t-tlb)/ ({difftime})  ) ) "/>'

  beta = '<definition id="Definition.beta" spec="phydyn.model.Definition" value=" beta = max(0, propub * betaub + (1-propub)*betalb) "/>'

  glue( paste( sep = '\n', betalb, betaub, tlb, propub, beta  ) )
}
