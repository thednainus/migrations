# Configure a model with exponential growth, birth(beta) & death(gamma), in two demes, with migration (mu) between demes
eqns <- list(
  confeqn( 'beta*gb', type = 'birth', origin = 'gb', destination = 'gb' )
  , confeqn( 'beta*oth', type = 'birth', origin = 'oth', destination = 'oth' )
  , confeqn( 'mu*oth', type = 'migration', origin = 'oth', destination = 'gb' )
  , confeqn( 'mu*gb', type = 'migration', origin = 'gb', destination = 'oth' )
  , confeqn( 'gamma*gb', type = 'death', origin = 'gb' )
  , confeqn( 'gamma*oth', type = 'death', origin = 'oth' )
)
parms <- list(
  confparm( 'beta'
            , initial = 1.5*365*(1/5.5)
            , prior = 'lognormal'
            , operator = 'realrw'
            , lower = .5*365*(1/5.5)
            , upper = 4*365*(1/5.5)
            , M = log(  1.5*365*(1/5.5)) , S = 1.0
  )
  , confparm( 'gamma'
              , initial = 365*(1/5.5)
              , estimate = FALSE
  )
  , confparm( 'mu'
              , initial = 12
              , prior = 'exponential'
              , operator = 'realrw'
              , estimate = TRUE
              , lower = 1/4
              , upper = 52
              , mean = 12
  )
  # estimate initial deme sizes
  , confparm( 'gb'
              , initial_condition_parameter = TRUE
              , initial = 1e-2
              , estimate = TRUE
              , prior = 'exponential'
              , operator = 'realrw'
              , lower = 0
              , upper = 1e2
              , mean = 10
  )
  , confparm( 'oth'
              , initial_condition_parameter = TRUE
              , initial = 1e-2
              , estimate = TRUE
              , prior = 'exponential'
              , operator = 'realrw'
              , lower = 0
              , upper = 1e2
              , mean = 10
  )
)
model <- config_phydyn(
  system.file( 'extdata', 'ba.2.86_algnWu-Hu-1.1_qc0_beast_template.xml',
               package = 'phydynbeast' ),
  saveto = 'ba.2.86_simodel0.xml',
  t0 = confparm( 't0', estimate = FALSE, initial = 2023.25),
  equations = eqns,
  parameters = parms,
  coalescent_approximation = 'PL1',
  integrationSteps = 100,
  minP = 0.001,
  penaltyAgtY= 0,
  useStateName = TRUE,
  traj_log_file = 'simodel0-traj.tsv',
  traj_log_frequency = 10000
)
