#phydynR HIV model1
#model incidence as piecewise linear function

#in this model I am implementing two migration rates m1 and m2
#m1 is from src to I
#m2 is from I to src

library(phydynR)
library(akima)

#demes are I = infected individuals and src = source individuals to account for
#importation
demes <- c('I', 'src')
#nondemes <- 'T'
#non-demes are Tr = individuals on treatment
nondemes <- c('Tr')


#these initial values for beta were based on estimated transmission rates using
#ECDC data for the risks paper
parms <- list(src1980  =  1e4,
              I1980 = 1,
              beta1980 = 1.0,
              beta1995 = 0.02,
              beta2005 = 0.04,
              gamma = 1/5.06,
              srcMigrationRate = 1/10,
              srcGrowthRate = 1/3,
              art_start = 1995,
              alpha = 0.3 #rate of ART initiation
)

betaTimes <- c(1980, 1995, 2005)
betaNames <- paste0("beta", betaTimes)


parms$beta.t <- function(t, p){
  approx( betaTimes, unlist( p[ betaNames ] ) , xout = t, rule = 2)$y
}

parms$alpha.t <- function(t, p){
  if ( t < p$art_start )
    return (0)
  approx( c( p$art_start, 2020), c(0, p$alpha), xout = t , rule = 2)$y
}


x0 <- c( src = 1e10, I = 1 , Tr = 0 )

births <- matrix('0', nrow = length(demes), ncol = length(demes))
rownames(births) = colnames(births) <- demes

migs <- matrix('0', nrow = length(demes), ncol = length(demes))
rownames(migs) = colnames(migs) <- demes

deaths <- rep(0, length(demes))
names(deaths) <- demes

nonDemeDynamics <- c()

births['src','src'] <- 'src * parms$srcGrowthRate'
births['I', 'I'] <- 'I * parms$beta.t(t, parms)'


migs['src', 'I'] <- 'I * parms$m1'
#migs['I', 'src'] <- 'I * parms$m2'

deaths['I'] <- 'I * parms$gamma + I * parms$alpha.t(t, parms)'


nonDemeDynamics['Tr'] <- 'I * parms$alpha.t(t, parms)'


dm <- build.demographic.process( births = births,
                                 migrations = migs,
                                 deaths = deaths,
                                 nonDemeDynamics = nonDemeDynamics,
                                 parameterNames = names(parms),
                                 sde = FALSE,
                                 rcpp = FALSE)



#t0t1 <- list(t0 = 1980 , t1 = 2020)

#quartz()
#show.demographic.process(dm,
#                         theta = parms,
#                         x0 = x0,
#                         t1 = t0t1$t1,
#                         t0 = t0t1$t0,
#                         integrationMethod = 'rk4' , res = 1000)

#s <-  dm(parms,  x0 , t0 = 1980, t1 = 2020, res = 100 )
#y1 <- tail( s[[5]], 1 )[1,]


#dm.run <- dm(theta = parms
# , x0 = x0
# , t0 = t0t1$t0
# , t1 = t0t1$t1
# , integrationMethod = 'rk4' , res = 1000
#)
