library(phydynbeast)

# Simple HIV model with migration between region and src
# I = infectious individuals in region
# Tr = individuals who are receiving treatment
# src = source compartment (comprising individuals from a global population)

# beta = transmission rate as linear equation
# alpha = the rate individuals start treatment also as a linear equation
# srcGrowthRate = rate in which the src population grow
# gamma = disease mortality rate
# srcMigrationRate = rate at which individuals migrate between src and region
# population

# Note that "less than" in the xml file should be written as !>

#to plot the continous piecewise linear equations and check that they are
# correct
# f_linear <- function(t){
#
#   a1980 <-  -7.78
#   b1980 <-  9.34
#   a2005 <- -4.19
#
#   a1995 <-  5.11
#   b1995 <- 6.22
#   #b2005 <- 10
#
#
#   if(t >= 1980 & t < 1995){
#     #a1980 * t + b1980
#     print("t >= 1980 & t < 1995")
#     max(0.0,a1980*t+b1980)
#   } else if (t >= 1995 & t < 2005){
#     print("t >= 1995 & t < 2005")
#     #b1980 + (a1980 - a1995) * 1995.0 + a1995 * t
#     max(0.0,b1980+(a1980-a1995)*1995.0+a1995*t)
#   } else {
#     print("last")
#     #(2005.0 * (a1995 - a2005)) + b1980 + (1995.0 * (a1980 - a1995)) + a2005 * t
#     max(0.0,(2005.0*(a1995-a2005))+b1980+(1995.0*(a1980-a1995))+a2005*t)
#   }
# }
#
# time <- seq(1980, 2020, by = 0.1)
# f <- lapply(time, function(x) f_linear(x))
#
# quartz()
# plot(time, unlist(f), type = 'l', col = 'blue')


eqns <- list(confeqn( 'beta = if (t>=1980) then max(0.0, a1980*t + b1980) else if ((t>=1995) and (t!>2005)) then max(0.0, b1980 + (a1980 - a1995) * 1995.0 + a1995 * t) else max(0.0, (2005.0 * (a1995 - a2005)) + b1980 + (1995.0 * (a1980 - a1995)) + a2005 * t)',
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
           sigma = 0.0001),
  confparm('a1980',
           initial = 0.00003,
           prior = 'normal',
           operator = 'realrw',
           lower = -1, #it was -Inf
           upper = 1,
           mean = 0,
           sigma = 0.00001),
  confparm('a1995',
           initial = -0.00006,
           prior = 'normal',
           operator = 'realrw',
           lower = -1,
           upper = 1,
           mean = 0,
           sigma = 40),
  confparm('a2005',
           initial = 0.00008,
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
           lower = 0,
           upper = 2,
           mean = 0,
           sigma = 1),
  confparm('b1980',
           initial = 0.1,
           prior = 'normal',
           operator = 'realrw',
           lower = 0,
           upper = 2,
           mean = 0,
           sigma = 1),
  confparm('b1995',
           initial = 0.20,
           prior = 'normal',
           operator = 'realrw',
           lower = 0,
           upper = 2,
           mean = 0,
           sigma = 1),
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

model <- config_phydyn(xmlfn = "phydynbeast_test/region1000global100_template.xml",
                       saveto = 'phydynbeast_test/new_test_ali/HIVsimple_model_newali_QL_modt0.xml',
                       t0 = confparm( 't0', estimate = FALSE, initial = 1980),
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

model <- config_phydyn(xmlfn = "phydynbeast_test/region1000global100_template.xml",
                       saveto = 'phydynbeast_test/new_test_ali/HIVsimple_model_newali_QL_modt0.xml',
                       t0parm = confparm(name ='t0', estimate = TRUE, initial = 1980,
                                      prior = 'normal',
                                      operator = 'realrw',
                                      lower = 1970,
                                      upper = 1990,
                                      mean = 1980,
                                      sigma = 1),
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

