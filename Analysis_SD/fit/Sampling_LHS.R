library(lhs)

sampler_lhs <- function(n = 10000, paramdf){

  d = length(names(paramdf)) # number of dimension (the number of parameters that we will estimate)
  params <- randomLHS(n, d)
  params[,1] <- qunif(params[,1], min = paramdf[1,1], max = paramdf[2,1])
  params[,2] <- qunif(params[,2], min = paramdf[1,2], max = paramdf[2,2])
  params[,3] <- qunif(params[,3], min = paramdf[1,3], max = paramdf[2,3])
  #params[,4] <- qunif(params[,4], min = paramdf[1,4], max = paramdf[2,4])
  #params[,5] <- qunif(params[,5], min = paramdf[1,5], max = paramdf[2,5])

  params <- data.frame(params)
  colnames(params) <- names(paramdf)


  return (params)

}

#sample parameters to simulate MSM epidemics

# parameters to sample:

# 1. inc_scale: incidence scale
# 2. max_diag_scale: maximum value of logistic function after 1985 (diagnosis rate)
# 3. accel_diag_rate: stepness of logistic function after 1985 (diagnosis rate)


#fit1_weeks_v2 imperial college cluster
#using all values for incidence and time resolution in weeks
#here I return the values of the incidence estimated with ECDC

params3dim_weeks_v1 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.01, 0.0),
                                                  max_diag_rate = c(0.01, 0.1),
                                                  accel_diag_rate = c(0.1, 1.5)
                             ))

params3dim_weeks_v2 <- sampler_lhs(n = 10000,
                                   paramdf = data.frame(inc_scale = c(0.008, 0.01),
                                                        max_diag_rate = c(0.1, 0.5),
                                                        accel_diag_rate = c(0.1, 1.5)
                                   ))

#it was a decent fit for incidence and a bit high values for incidence of
#diagnosis
params3dim_weeks_v3 <- sampler_lhs(n = 10000,
                                   paramdf = data.frame(inc_scale = c(0.01, 0.03),
                                                        max_diag_rate = c(0.3, 1.5),
                                                        accel_diag_rate = c(0.2, 1.5)
                                   ))

#trying to narrow down the parameter values space to get a better fit
params3dim_weeks_v4 <- sampler_lhs(n = 10000,
                                   paramdf = data.frame(inc_scale = c(0.015, 0.025),
                                                        max_diag_rate = c(0.2, 0.5),
                                                        accel_diag_rate = c(0.1, 0.4)
                                   ))

#trying to narrow down the parameter values space to get a better fit
params3dim_weeks_v5 <- sampler_lhs(n = 10000,
                                   paramdf = data.frame(inc_scale = c(0.02, 0.025),
                                                        max_diag_rate = c(0.23, 0.35),
                                                        accel_diag_rate = c(0.15, 0.35)
                                   ))

#trying to narrow down the parameter values space to get a better fit
params3dim_weeks_v6 <- sampler_lhs(n = 10000,
                                   paramdf = data.frame(inc_scale = c(0.01, 0.019),
                                                        max_diag_rate = c(0.23, 0.5),
                                                        accel_diag_rate = c(0.15, 1)
                                   ))

#trying to narrow down the parameter values space to get a better fit
params3dim_weeks_v7 <- sampler_lhs(n = 10000,
                                   paramdf = data.frame(inc_scale = c(0.01, 0.016),
                                                        max_diag_rate = c(0.2, 0.5),
                                                        accel_diag_rate = c(0.2, 0.5)
                                   ))




saveRDS(params3dim_weeks_v1, "inst/data/params3dim_weeks_v1.RDS")
saveRDS(params3dim_weeks_v2, "inst/data/params3dim_weeks_v2.RDS")
saveRDS(params3dim_weeks_v3, "inst/data/params3dim_weeks_v3.RDS")
saveRDS(params3dim_weeks_v4, "inst/data/params3dim_weeks_v4.RDS")
saveRDS(params3dim_weeks_v5, "inst/data/params3dim_weeks_v5.RDS")
saveRDS(params3dim_weeks_v6, "inst/data/params3dim_weeks_v6.RDS")
saveRDS(params3dim_weeks_v7, "inst/data/params3dim_weeks_v7.RDS")



