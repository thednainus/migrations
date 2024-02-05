library(phydynR)
library(ggplot2)
library(reshape2)

source("Analysis_SD/simulations/model0_SD_weeks.R")
source(system.file("data/incidence_HIVdiagnosis.R", package = "HIVepisimAnalysis"))

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

newDx_pop1_agg <- aggregate(newDx_pop1 ~ only_year ,
                            data = care2,
                            FUN=sum)


newDx_pop1_agg$only_year <- as.numeric(newDx_pop1_agg$only_year)
names(newDx_pop1_agg)[2] <- "diagnosed"
newDx_pop1_agg <- newDx_pop1_agg[c(6:41),]

quartz()
names(incidenceDiag)[1] <- "only_year"
incidenceDiag$only_year <- as.numeric(incidenceDiag$only_year)
incidenceDiag <- incidenceDiag[c(6:41),]
#incidenceDiag <- incidenceDiag[6:41,]
ggplot(newDx_pop1_agg, aes(x=only_year)) +
  geom_line(aes(y = diagnosed, colour = "Simulated data")) +
  geom_line(data = incidenceDiag, aes(y = frequency, colour = "San Diego surveillance data"),
            linewidth = 0.8) +
  theme_bw() + ylab("Incidence of diagnosis") +
  xlab("Year") +
  theme(text = element_text(size=8), legend.position = "bottom") +
  theme(legend.title=element_blank())


