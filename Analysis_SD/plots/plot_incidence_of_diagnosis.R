library(phydynR)




best_runs1 <- list.files("fit/fit1", full.names = TRUE, pattern = "param")
best_runs2 <- list.files("fit/fit2", full.names = TRUE, pattern = "param")
best_runs3 <- list.files("fit/fit3", full.names = TRUE, pattern = "param")
best_runs4 <- list.files("fit/fit4", full.names = TRUE, pattern = "param")
#best_runs5 <- list.files("fit/fit5", full.names = TRUE, pattern = "param")
best_runs6 <- list.files("fit/fit6", full.names = TRUE, pattern = "param")
best_runs7 <- list.files("fit/fit7", full.names = TRUE, pattern = "param")
best_runs8 <- list.files("fit/fit8", full.names = TRUE, pattern = "param")
best_runs9 <- list.files("fit/fit9", full.names = TRUE, pattern = "param")
best_runs10 <- list.files("fit/fit10", full.names = TRUE, pattern = "param")


best_runs1_weeks <- list.files("fit/fit1_weeks", full.names = TRUE, pattern = "param")

best_runs3_weeks <- list.files("fit/fit3_weeks", full.names = TRUE, pattern = "param")
best_runs4_weeks <- list.files("fit/fit4_weeks", full.names = TRUE, pattern = "param")







best_runs2 <- c(best_runs6, best_runs7, best_runs8, best_runs9, best_runs10)

best_runs2 <- c(best_runs8, best_runs1_weeks)


all_best_runs <- data.frame()

for (i in 1:length(best_runs2)){

  #load("best_runs3/param_984_inc_scale_1.26621755970543e-05_max_diag_rate_0.00080434082739912_accel_diag_rate_15.1500348115712.rda")
  load(best_runs2[i])

  if(i > 1){

    care2 <- aggregate(newDx_pop1  ~ only_year, data = care2, FUN=sum)

    new_diagnosis <- care2[c(6:41),]
    new_diagnosis["param"] <- i

  }else{
    new_diagnosis <- care2[c(6:41),3:2]
    new_diagnosis["param"] <- i
  }



  #if(i > 7){
  #  new_diagnosis <- newDx_pop1_agg[c(6:41),]
  #  new_diagnosis["param"] <- i
  #}


  all_best_runs <- rbind(all_best_runs, new_diagnosis)
}




source(system.file("data/incidence_HIVdiagnosis.R", package = "HIVepisimAnalysis"))
#incidence <- readRDS(system.file("data/ECDC_incidence_model_22Oct2021.RDS",
#                                 package = "HIVepisimAnalysis"))
library(ggplot2)
library(reshape2)

all_best_runs$only_year <- as.numeric(all_best_runs$only_year)
all_best_runs$param <- as.factor(all_best_runs$param)
names(all_best_runs)[2] <- "diagnosed"

quartz()
names(incidenceDiag)[1] <- "only_year"
incidenceDiag$only_year <- as.numeric(incidenceDiag$only_year)
incidenceDiag <- incidenceDiag[c(6:41),]
#incidenceDiag <- incidenceDiag[6:41,]
ggplot(all_best_runs, aes(x=only_year)) +
  geom_line(aes(y = diagnosed, colour = param)) +
  geom_line(data = incidenceDiag, aes(y = frequency, colour = "San Diego data"),
            linewidth = 0.8) +
  theme_bw() + ylab("Incidence of diagnosis") +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())

quartz()
names(incidenceDiag)[1] <- "year"
incidenceDiag$year <- as.numeric(incidenceDiag$year)
incidenceDiag <- incidenceDiag[c(3:41),]
#incidenceDiag <- incidenceDiag[6:41,]
ggplot(all_best_runs, aes(x=year)) +
  geom_line(aes(y = diagnosed)) +
  geom_line(data = incidenceDiag, aes(y = frequency, colour = "San Diego data"),
            linewidth = 0.8) +
  geom_line(data = incidence, aes(y = model1, colour = "model1"),
            linewidth = 0.8) +
  theme_bw() + ylab("Incidence of diagnosis") +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())


quartz()
names(incidenceDiag)[1] <- "only_year"
incidenceDiag$only_year <- as.numeric(incidenceDiag$only_year)
#incidenceDiag <- incidenceDiag[6:41,]
ggplot(newDx_pop1_agg, aes(x=only_year)) +
  geom_line(aes(y = newDx_pop1)) +
  geom_line(data = incidenceDiag, aes(y = frequency, colour = "San Diego data"),
            size = 0.8) +
  theme_bw() + ylab("Incidence of diagnosis") +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())

