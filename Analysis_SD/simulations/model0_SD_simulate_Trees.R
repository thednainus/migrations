library(lubridate)
library(SDBenefits)



#source('Analysis_SD/simulations/model0_SD_weeks.R')
#source('Analysis_SD/simulations/model0_SD_sampleTimes_and_States.R')

source('Analysis_SD/simulations/model0_SD_weeks.R')
source('Analysis_SD/simulations/model0_SD_sampleTimes_and_States.R')



o <- ode(y=y0, times=times_day, func=dydt, parms=list()  , method = 'adams')
tfgy <- .tfgy( o )

#simulate phylogenetic trees
print('sim tree')
print(date())
#start of script
start_time <- Sys.time()
st.tree <- system.time( {
  daytree <- sim.co.tree.fgy(tfgy, st_days, ss)
})
print(date())
#end of script
end_time <- Sys.time()
print("Simulation of trees took:")
end_time - start_time

#quartz()
#plot(daytree, show.tip.label = FALSE)
#plot(daytree, show.tip.label = TRUE)
#axisPhylo()

# rescale tree
tree <- daytree
sampleTimes <- days2years(tree$sampleTimes )
tree$edge.length <- tree$edge.length / 365


#quartz()
#plot(tree, show.tip.label = TRUE)
#axisPhylo()

save(daytree, tree, tfgy, st_days, ss, file = "Analysis_SD/simulations/trees.rda")

tree_time <- data.frame(start = start_time, end = end_time)
saveRDS(tree_time, "tree_time.RDS")

