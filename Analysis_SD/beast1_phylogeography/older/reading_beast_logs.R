#read beast1 log file

beast1_log <- read.delim("~/Desktop/beast1_results/importation_rate/exponential/medium/region100global50/run1/results/run_1/phylogeography_beast1.log", sep = "\t", skip = 3)

quartz()
plot(beast1_log$state, beast1_log$exponential.popSize)

beast1_log.1 <- read.delim("~/Desktop/beast1_results/importation_rate/exponential/medium/region50global150/run1/results/run_1/phylogeography_beast1.log", sep = "\t", skip = 3)

plot(beast1_log.1$state, beast1_log.1$exponential.popSize)

quartz()
plot(beast1_log.1$state[2500:5001], beast1_log.1$exponential.popSize[2500:5001])
