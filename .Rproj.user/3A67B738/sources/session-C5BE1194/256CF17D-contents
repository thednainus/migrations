library(phydynR)

o <- ode(y=y0, times=times_day, func=dydt, parms=list()  , method = 'adams')
tfgy <- .tfgy( o )

sizes <- do.call(rbind, tfgy[[4]])

times_sizes <- cbind(tfgy[[1]], sizes)
times_sizes_df <- as.data.frame(times_sizes)
times_sizes_df["year"] <- unlist(lapply(times_sizes_df[,1], function(x) days2years(x)))
grepl(pattern = "care2", x = names(times_sizes_df))


#diagnosed individuals (cumulative diagnosis)
care2 <- times_sizes_df[,which(grepl(pattern = "care2", x = names(times_sizes_df)))]
care2["times"] <- times_sizes_df$year

care2_sum <- apply(care2, 1, sum)

care2_total <- data.frame(time = care2$times, diagnosed = care2_sum)


mig <- do.call(rbind, tfgy[[3]])
times_mig <- cbind(tfgy[[1]], mig)
times_mig_df <- as.data.frame(times_mig)
times_mig_df["year"] <- unlist(lapply(times_mig_df[,1], function(x) days2years(x)))
grepl(pattern = "care2", x = names(times_mig_df))


#diagnosed individuals (cumulative diagnosis)
care2_mig <- times_mig_df[,which(grepl(pattern = "care2", x = names(times_mig_df)))]
care2_mig["times"] <- times_mig_df$year

care2_mig_sum <- apply(care2_mig, 1, sum)

care2_mig_total <- data.frame(time = care2_mig$times, diagnosed = care2_mig_sum)





source(system.file("data/incidence_HIVdiagnosis.R", package = "HIVepisimAnalysis"))
incidence <- readRDS(system.file("data/ECDC_incidence_model_22Oct2021.RDS",
                                 package = "HIVepisimAnalysis"))
library(ggplot2)
library(reshape2)


quartz()
names(incidenceDiag)[1] <- "year"
incidenceDiag$year <- as.numeric(incidenceDiag$year)
#incidenceDiag <- incidenceDiag[6:41,]
ggplot(care2_total, aes(x=year)) +
  geom_line(aes(y = value, linetype = variable, colour = param_migrant), linetype = 2) +
  theme_bw() + ylab("Incidence of diagnosis") +
  scale_fill_manual(values=c("#c9222a", "#222ac9"), guide = "none") +
  scale_color_manual(values=c("#c9222a", "#222ac9", "black"),
                     breaks=c("1067_500", "2348_500", "San Diego data"),
                     labels=c("Parameters 1", "Parameters 2", "San Diego data")) +
  theme(text = element_text(size=20), legend.position = "bottom") +
  theme(legend.title=element_blank())

