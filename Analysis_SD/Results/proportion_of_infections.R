#proportion of infections attributable to "src" or "global"
#which is the same as proportion of infections attributable to migrants

library(SDBenefits)



source('Analysis_SD/simulations/model0_SD_weeks.R')
#source('model0_SD_weeks.R')

o <- ode(y=y0, times=times_day, func=dydt, parms=list()  , method = 'adams')
tfgy <- .tfgy( o )

times_year <- unlist(lapply(tfgy[[1]], function(x) days2years(x)))

#tfgy[[3]] corresponds to migration matrix
src2region <- unlist(lapply(tfgy[[3]],
                     function(x) round(sum(x[121,1:120]), 6)/round(sum(x[1:121,1:120]), 6)))



src2region_df <- data.frame(year = times_year, src2region = src2region)

plot(src2region_df$year[-1], src2region_df$src2region[-1])

src_size <- unlist(lapply(tfgy[[4]],
                          function(x) x[121]))
src_size_df <- data.frame(year = times_year, src_size = src_size)
plot(src_size_df$year, src_size_df$src_size)
