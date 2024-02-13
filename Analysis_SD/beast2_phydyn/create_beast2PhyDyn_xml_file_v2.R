library(phydynbeast)

# Simple HIV model with migration between region and src
# I = infectious individuals in region
# Tr = individuals who are receiving treatment
# src = source compartment (comprising individuals from a global population)

# beta = transmission rate as ??
# alpha = the rate individuals start treatment also as a linear equation
# srcGrowthRate = rate in which the src population grow
# gamma = disease mortality rate
# srcMigrationRate = rate at which individuals migrate between src and region
# population

# Note that "less than" in the xml file should be written as !>

#to plot the continous piecewise linear equations and check that they are
# correct

#adding equations to create a beast2 phydyn xml file using the R package
#phydynbeast (https://github.com/emvolz/phydynbeast)
eqns <- list(confeqn( 'beta1 = if (t !> 1980) then 0.0 else (max (0.0, b1))',
                      type = 'definition'),
             confeqn( 'betalb = if (t !> 1980 ) then 0.0 else if (t !>= 1995) then beta1 else if (t !>= 2005) then beta1 * exp( dlogbeta[0]) else (beta1 * exp( dlogbeta[0] + dlogbeta[1])',
                      type = 'definition'),
             confeqn('betaub = if ( t !> 1980) then 0.0 else if (t !>= 1995) then beta1 * exp( dlogbeta[0] ) else if (t !>= 2005) then beta1 * exp( dlogbeta[0] + dlogbeta[1]) else (beta1 * exp( dlogbeta[0] + dlogbeta[1] + dlogbeta[2] ))',
                     type = 'definition'),
             confeqn('tlb  = if (t !> 1980 ) then 1980 else if (t !>= 1995) then 1980 else if (t !>= 2005) then 1995 else (T1)',
                     type = 'definition'),
             confeqn('propub = max(0, min(1, (t-tlb)/ (T1 - 2005)',
                     type = 'definition'),
             confeqn('beta = max(0, propub * betaub + (1-propub)*betalb)',
                     type = 'definition'),
             confeqn( 'alpha = if ((t!>1995)) then 0.0 else max(0.0, a*t + b)',
                      type = 'definition'),
             confeqn( 'srcGrowthRate*src', type = 'birth',
                      origin = 'src', destination = 'src'),
             confeqn( 'beta*I', type = 'birth',
                      origin = 'I', destination = 'I'),
             confeqn( 'srcMigrationRate*I', type = 'migration',
                      origin = 'src', destination = 'I'),
             confeqn( 'gamma*I + alpha*I', type = 'death',
                      origin = 'I'),
             confeqn( 'alpha*I', type = 'nondeme',
                      origin = 'Tr')
)
parms <- list(
  confparm('a',
           initial = 0.0001,
           prior = 'normal',
           operator = 'realrw',
           lower = -1,
           upper = 1,
           mean = 0,
           sigma = 0.00001),
  confparm('b',
           initial = 0.05,
           prior = 'normal',
           operator = 'realrw',
           lower = 0.001,
           upper = 2,
           mean = 0,
           sigma = 0.3),
  confparm('b1',
           initial = 1,
           prior = 'normal',
           operator = 'realrw',
           lower = 0,
           upper = 2,
           mean = 0,
           sigma = 2),
  confparm('dlogbeta',
           initial = 0,
           prior = 'normal',
           operator = 'realrw',
           mean = 0,
           sigma = 1/2,
           isVector=TRUE,
           dimension='3'), #I don't know where I should add this
  confparm('gamma',
           initial = 1/5.06,
           estimate = FALSE),
  confparm('srcGrowthRate',
           initial = 1/3,
           estimate = FALSE),
  confparm('srcMigrationRate',
           initial = 0.1,
           prior = 'lognormal',
           operator = 'realrw',
           estimate = TRUE,
           lower = 1/24,
           upper = 1,
           M = log(1/20),
           S = 1/4),

  # estimate initial deme sizes
  confparm('I',
           initial_condition_parameter = TRUE,
           initial = 1,
           estimate = TRUE,
           prior = 'exponential',
           operator = 'realrw',
           lower = 1,
           upper = 500,
           mean = 1/5),

  confparm('src',
           initial_condition_parameter = TRUE,
           initial = 1e4,
           estimate = TRUE,
           prior = 'lognormal',
           operator = 'realrw',
           lower = 1,
           upper = 1e5,
           M = log(1.5e3),
           S = 1/2),

  confparm('Tr',
           initial_condition_parameter = TRUE,
           initial = 0,
           estimate = FALSE)
)

# model <- config_phydyn(xmlfn = "phydynbeast_test/region1000global100_template.xml",
#                        saveto = 'phydynbeast_test/new_test_ali/HIVsimple_model_newali_QL_modt0.xml',
#                        t0parm = confparm(name ='t0', estimate = TRUE, initial = 1980,
#                                       prior = 'normal',
#                                       operator = 'realrw',
#                                       lower = 1970,
#                                       upper = 1990,
#                                       mean = 1980,
#                                       sigma = 1),
#                        equations = eqns,
#                        parameters = parms,
#                        coalescent_approximation = 'QL',
#                        integrationSteps = 100,
#                        minP = 0.001,
#                        penaltyAgtY= 0,
#                        useStateName = TRUE,
#                        traj_log_file = 'simodel0-traj_linear.tsv',
#                        traj_log_frequency = 1000
# )

model <- config_phydyn(xmlfn = "phydynbeast_test/region1000global100_template.xml",
                       saveto = 'phydynbeast_test/new_test_ali/HIVsimple_model_newali_QL_v2.xml',
                       t0parm = confparm(name ='t0', estimate = FALSE, initial = 1977),
                       equations = eqns,
                       parameters = parms,
                       coalescent_approximation = 'QL',
                       integrationSteps = 100,
                       minP = 0.001,
                       penaltyAgtY= 0,
                       useStateName = TRUE,
                       traj_log_file = 'simodel0-traj_linear.tsv',
                       traj_log_frequency = 1000
)

