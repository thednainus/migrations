library(phydynR)
library(ggplot2)
library(reshape2)


source(system.file("data/incidence_HIVdiagnosis.R", package = "HIVepisimAnalysis"))
incidence <- readRDS(system.file("data/ECDC_incidence_model_22Oct2021.RDS",
                                 package = "HIVepisimAnalysis"))
names(incidence) <- c("only_year", "incidence")
names(incidenceDiag)[1] <- "year"



quartz()
ggplot(incidence, aes(x=year)) +
  geom_line(aes(y = incidence, colour = "incidence")) +
  geom_line(data = incidenceDiag, aes(y = frequency, colour = "incidence diagnosis"),
            linewidth = 0.8) +
  theme_bw() + ylab("San Diego Data") +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())



