#proportion of infections attributable to src or "global"
#which is the proportion of infections attributable to migrants

library(SDBenefits)



source('Analysis_SD/simulations/model0_SD_weeks.R')
#source('model0_SD_weeks.R')

o <- ode(y=y0, times=times_day, func=dydt, parms=list()  , method = 'adams')
tfgy <- .tfgy( o )

times_year <- unlist(lapply(tfgy[[1]], function(x) days2years(x)))

src2region <- lapply(tfgy[[3]],
                     function(x) round(x[121,1:120], 6)/round(sum(x[1:121,1:120]), 6))
src2region_sum <- unlist(lapply(src2region, function(x) sum(x)))




src2region_df <- data.frame()



