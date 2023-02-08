library(lhs)

sampler_lhs <- function(n = 1000, paramdf){

  d = length(names(paramdf)) # number of dimension (the number of parameters that we will estimate)
  params <- randomLHS(n, d)
  params[,1] <- qunif(params[,1], min = paramdf[1,1], max = paramdf[2,1])
  params[,2] <- qunif(params[,2], min = paramdf[1,2], max = paramdf[2,2])
  params[,3] <- qunif(params[,3], min = paramdf[1,3], max = paramdf[2,3])
  params[,4] <- qunif(params[,4], min = paramdf[1,4], max = paramdf[2,4])
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

#fit1 imperial college cluster
#for fit1 I provided all points for the estimated incidence
#but best runs for new diagnosis followed the similar trajectory as incidence
#creating that "two peaks"
params3dim_v1 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.001, 1),
                                                  max_diag_rate = c(0.001, 1.5),
                                                  accel_diag_rate = c(0.01, 1.5)
                             ))

#fit2 imperial college cluster
#still quite no right the time in which the peak appears
params3dim_v2 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.001, 1.5),
                                                  max_diag_rate = c(0.01, 0.1),
                                                  accel_diag_rate = c(0.01, 1)
                             ))

#fit3 imperial college cluster
params3dim_v3 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.001, 1.8),
                                                  max_diag_rate = c(0.001, 1.5),
                                                  accel_diag_rate = c(0.01, 1.5)
                             ))

#fit4 imperial college cluster
params3dim_v4 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.01, 1.8),
                                                  max_diag_rate = c(0.01, 1),
                                                  accel_diag_rate = c(0.01, 1.5)
                             ))

#fit6 imperial college cluster
#trying to determine the best number of points to use from incidence curve
params4dim_v1 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.01, 1.8),
                                                  max_diag_rate = c(0.01, 1),
                                                  accel_diag_rate = c(0.01, 1.5),
                                                  line_incidence = c(6, 41)
                             ))

params4dim_v1$line_incidence <- round(params4dim_v1$line_incidence)


#fit7 imperial college cluster
#trying to determine the best number of points to use from incidence curve
params4dim_v2 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.1, 3),
                                                  max_diag_rate = c(0.1, 2),
                                                  accel_diag_rate = c(0.01, 2),
                                                  line_incidence = c(6, 41)
                             ))

params4dim_v2$line_incidence <- round(params4dim_v2$line_incidence)


#fit8 imperial college cluster
#using all the points from incidence curve
params3dim_v5 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.5, 2),
                                                  max_diag_rate = c(0.08, 1),
                                                  accel_diag_rate = c(0.02, 3)
                             ))

#fit9 imperial college cluster
#using all the points from incidence curve
#here I will narrow down the parameters a lot to the three best fits
#fit6, 7 and 8
params3dim_v6 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(1.1, 1.5),
                                                  max_diag_rate = c(0.09, 0.2),
                                                  accel_diag_rate = c(0.01, 2)
                             ))


#fit10 imperial college cluster
#trying to determine the best number of points to use from incidence curve
params4dim_v3 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.1, 3),
                                                  max_diag_rate = c(0.1, 2),
                                                  accel_diag_rate = c(0.01, 3),
                                                  line_incidence = c(30, 41)
                             ))

params4dim_v3$line_incidence <- round(params4dim_v3$line_incidence)

#fit11 imperial college cluster
#trying to determine the best number of points to use from incidence curve
params4dim_v4 <- sampler_lhs(n = 10000,
                             paramdf = data.frame(inc_scale = c(0.9, 2),
                                                  max_diag_rate = c(0.1, 0.5),
                                                  accel_diag_rate = c(0.01, 2),
                                                  line_incidence = c(30, 41)
                             ))

params4dim_v4$line_incidence <- round(params4dim_v4$line_incidence)






saveRDS(params3dim_v1, "inst/data/params3dim_v1.RDS")
saveRDS(params3dim_v2, "inst/data/params3dim_v2.RDS")
saveRDS(params3dim_v3, "inst/data/params3dim_v3.RDS")
saveRDS(params3dim_v4, "inst/data/params3dim_v4.RDS")
saveRDS(params4dim_v1, "inst/data/params4dim_v1.RDS")
saveRDS(params4dim_v2, "inst/data/params4dim_v2.RDS")
saveRDS(params3dim_v5, "inst/data/params3dim_v5.RDS")
saveRDS(params3dim_v6, "inst/data/params3dim_v6.RDS")
saveRDS(params4dim_v3, "inst/data/params4dim_v3.RDS")
saveRDS(params4dim_v4, "inst/data/params4dim_v4.RDS")

