#plot some trajectories using the best combination of parameter values
#using LHS

library(ggplot2)
library(reshape2)

source("Analysis_SD/model0_SD.R")

#estimate tfgy to fit model to SD new diagnosis
o <- ode(y=y0, times=times_day, func=dydt, parms=list()  , method = 'adams')
tfgy <- .tfgy( o )



#get number of individuals from care1 (undiagnosed) to
#care 2 (diagnosed)
GG_care1_to_care2 <- lapply(tfgy[[3]], function(x) x[which(grepl(pattern = "care1",
                                                                 x = rownames(x))),
                                                     which(grepl(pattern = "care2",
                                                                 x = colnames(x)))])

#This sum will give the total per time step
GG_sum <- unlist(lapply(GG_care1_to_care2, function(x) sum(x)))



new_diagnosis <- GG_sum
new_diagnosis_year <- unlist(lapply(tfgy[[1]], function(x) days2years(x)))

#care2 = new diagnosis
care2 <- data.frame(year = new_diagnosis_year, diagnosed = new_diagnosis)
care2["only_year"] <- unlist(lapply(care2$year, function(x) strsplit(as.character(x), split = ".", fixed = TRUE)[[1]][1]))

names(care2)[2] <- "newDx_pop1"
care2$newDx_pop1 <- round(care2$newDx_pop1)

new_diagnosis <- care2[c(6:41),3:2]
new_diagnosis["param"] <- "best_fit"


## plot incidence of diagnosis ----
source(system.file("data/incidence_HIVdiagnosis.R", package = "HIVepisimAnalysis"))
#incidence <- readRDS(system.file("data/ECDC_incidence_model_22Oct2021.RDS",
#                                 package = "HIVepisimAnalysis"))


new_diagnosis$only_year <- as.numeric(new_diagnosis$only_year)
new_diagnosis$param <- as.factor(new_diagnosis$param)
names(new_diagnosis)[2] <- "diagnosed"

quartz()
names(incidenceDiag)[1] <- "only_year"
incidenceDiag$only_year <- as.numeric(incidenceDiag$only_year)
incidenceDiag <- incidenceDiag[c(6:41),]
#incidenceDiag <- incidenceDiag[6:41,]
ggplot(new_diagnosis, aes(x=only_year)) +
  geom_line(aes(y = diagnosed, colour = param)) +
  geom_line(data = incidenceDiag, aes(y = frequency, colour = "San Diego data"),
            linewidth = 0.8) +
  theme_bw() + ylab("Incidence of diagnosis") +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())

## plotting incidence (new infections) ----


#This sum will give the total per time step
births <- unlist(lapply(tfgy[[2]], function(x) sum(x[,-121])))
incidence_year <- unlist(lapply(tfgy[[1]], function(x) days2years(x)))

#incidence = new infections
incidence <- data.frame(year = incidence_year, incidence = births)
incidence["only_year"] <- unlist(lapply(incidence$year, function(x) strsplit(as.character(x), split = ".", fixed = TRUE)[[1]][1]))

incidence$only_year <- as.numeric(incidence$only_year)

inc_agg <- aggregate(incidence ~ only_year ,
                            data = incidence,
                            FUN=sum)


incidence_ecdc <- readRDS(system.file("data/ECDC_incidence_model_22Oct2021.RDS",
                                 package = "HIVepisimAnalysis"))


quartz()
names(incidence_ecdc) <- c("only_year", "incidence")
ggplot(inc_agg, aes(x=only_year)) +
  geom_line(aes(y = incidence, colour = "simulated data")) +
  geom_line(data = incidence_ecdc, aes(y = incidence, colour = "incidence ecdc"),
            linewidth = 0.8) +
  theme_bw() + ylab("Incidence") +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())




## migrations to source compartment ----

#This sum will give the total per time step
birthssrc <- unlist(lapply(tfgy[[2]], function(x) sum(x[,121])))
year <- unlist(lapply(tfgy[[1]], function(x) days2years(x)))

#care2 = new diagnosis
births_src <- data.frame(year = year, births_src = birthssrc)
births_src["only_year"] <- unlist(lapply(births_src$year, function(x) strsplit(as.character(x), split = ".", fixed = TRUE)[[1]][1]))

births_src$only_year <- as.numeric(births_src$only_year)


quartz()
ggplot(births_src, aes(x=only_year)) +
  geom_line(aes(y = births_src)) +
  theme_bw() + ylab("Births src") +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())


## plot number of infections (Y) ----


#This sum will give the total per time step
infections <- unlist(lapply(tfgy[[4]], function(x) sum(x[-121])))
year <- unlist(lapply(tfgy[[1]], function(x) days2years(x)))

#incidence = new infections
Y <- data.frame(year = year, infections = infections)
Y["only_year"] <- unlist(lapply(Y$year, function(x) strsplit(as.character(x), split = ".", fixed = TRUE)[[1]][1]))

Y$only_year <- as.numeric(Y$only_year)


quartz()
ggplot(Y, aes(x=only_year)) +
  geom_line(aes(y = infections)) +
  theme_bw() + ylab("Infections") +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())

