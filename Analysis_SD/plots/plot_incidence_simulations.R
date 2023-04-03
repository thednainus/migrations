library(phydynR)
library(ggplot2)

## time axes & funcs
time_res <-  52 * (2020.997 - 1980)  # time steps / week
year0 <- 1980
year1 <- 2020.997
date0 <- as.Date('1980-01-01')
date1 <- as.Date('2020-12-31')
times0 <- 0
times1 <- as.numeric( date1 - date0 )
times_year <- seq(year0, year1, length.out = time_res) #to end of 2021
times_day <- seq( 0, times1, length.out = time_res )



days2years <- function( d ){
  year0  + (year1 - year0) * d / (times1 - 0 )
}
years2days <- function(y)
{
  (times1 - times0) * (y - year0) / (year1 - year0)
}

best_runs1_weeks <- list.files("fit/fit1_weeks_v2", full.names = TRUE, pattern = "param")
best_runs2_weeks <- list.files("fit/fit2_weeks_v2", full.names = TRUE, pattern = "param")
best_runs3_weeks <- list.files("fit/fit3_weeks_v2", full.names = TRUE, pattern = "param")
best_runs4_weeks <- list.files("fit/fit4_weeks_v2", full.names = TRUE, pattern = "param")
best_runs4_weeks <- best_runs4_weeks[1]
best_runs5_weeks <- list.files("fit/fit5_weeks_v2", full.names = TRUE, pattern = "param")
best_runs5_weeks <- best_runs5_weeks[1]
best_runs6_weeks <- list.files("fit/fit6_weeks_v2", full.names = TRUE, pattern = "param")
best_runs7_weeks <- list.files("fit/fit7_weeks_v2", full.names = TRUE, pattern = "param")
best_runs7_weeks <- best_runs7_weeks[1]



best_runs2 <- c(best_runs3_weeks, best_runs4_weeks, best_runs5_weeks, best_runs6_weeks, best_runs7_weeks)
best_runs2 <- best_runs6_weeks

all_best_runs <- data.frame()

for (i in 1:length(best_runs2)){

  load(best_runs2[i])

  births <- unlist(lapply(tfgy[[2]], function(x) sum(x[,-121])))
  incidence_year <- unlist(lapply(tfgy[[1]], function(x) days2years(x)))

  #incidence = new infections
  incidence <- data.frame(year = incidence_year, incidence = births)
  incidence["only_year"] <- unlist(lapply(incidence$year, function(x) strsplit(as.character(x), split = ".", fixed = TRUE)[[1]][1]))

  incidence$only_year <- as.numeric(incidence$only_year)

  inc_agg <- aggregate(incidence ~ only_year ,
                       data = incidence,
                       FUN=sum)

  inc_agg["param"] <- i



  all_best_runs <- rbind(all_best_runs, inc_agg)
}

all_best_runs$param <- as.factor(all_best_runs$param)


incidence_ecdc <- readRDS(system.file("data/ECDC_incidence_model_22Oct2021.RDS",
                                      package = "HIVepisimAnalysis"))


quartz()
names(incidence_ecdc) <- c("only_year", "incidence")
ggplot(all_best_runs, aes(x=only_year)) +
  geom_line(aes(y = incidence, colour = param)) +
  geom_line(data = incidence_ecdc, aes(y = incidence, colour = "incidence ecdc"),
            linewidth = 0.8) +
  theme_bw() + ylab("Incidence") +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())

