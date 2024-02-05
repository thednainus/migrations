
log_files <- list.files("/rds/general/user/fferre15/ephemeral/SD_Benefits/fit1/logs",
                        full.names = TRUE, pattern = "log_weights_dx_df")

log_weights_dx_df_all <- data.frame()

for (i in 1:length(log_files)){

  log_weights_dx_df <- readRDS(log_files[i])

  log_weights_dx_df_all <- rbind(log_weights_dx_df_all, log_weights_dx_df)
}


log_weights_dx_df_all["params"] <- rep(1:nrow(log_weights_dx_df_all))


newdata <- log_weights_dx_df_all[order(log_weights_dx_df_all$log_weights),]

w =   exp( log_weights_dx_df_all[,5]  - max( log_weights_dx_df_all[,5] ) )
resample <- sample( as.character( log_weights_dx_df_all[, 'params']  ), prob = w, replace = TRUE )

unique(resample)

log_weights_dx_df_all[unique(resample),]

saveRDS(log_weights_dx_df_all, "log_weights_dx_df_all.RDS")
