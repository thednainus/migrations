library(lubridate)
library(SDBenefits)

#total number of individuals that we will simulate for region
TAXA_SIZE_REGION = 10

#total number of sequences that we will simulate for source (global
#sequences)
TAXA_SIZE_SRC = 5


#sample DEMES (without source)
#random select state of individuals that will be in the matrix as 1

ss_region <- sample_states(n_compartments = 120,
                            n_size = TAXA_SIZE_REGION,
                            compartment_names = DEMES[-121])
ss_region <- cbind(ss_region, src = 0)

tip_names_region <- create_tip_names(ss_region, origin = "region")

rownames(ss_region) <- tip_names_region

#now add total number of individuals that we will simulate in the src
#compartment

ss_src <- matrix(0, ncol = length(DEMES), nrow = TAXA_SIZE_SRC)
ss_src[,121] <- 1
colnames(ss_src) <- DEMES


tip_names_src <- create_tip_names(ss_src, origin = "src")
rownames(ss_src) <- tip_names_src


#join the two matrix together
ss <- rbind(ss_region, ss_src)

#regularise
ss <- ss + 1e-4
ss = ss / rowSums(ss)


#sample times in years and then convert to days

#Times for sampling IDs and sampling times for within region
#sampling within the last 15 years
start_date <- ymd("2005-01-01")
start_date_dec <- decimal_date(start_date)
end_date <- ymd('2020-12-31')
end_date_dec <- decimal_date(end_date)
start_simulation <- ymd(date0)
start_simulation_dec <- decimal_date(start_simulation)

#sample times for region
st_region <- runif(n = TAXA_SIZE_REGION, min = start_date_dec,
                   max = end_date_dec)


#sample times for src
st_src <- runif(n = TAXA_SIZE_SRC, min = start_simulation_dec, max = end_date_dec)

#join the sample times for for region and source

st <- c(st_region, st_src)


rownames(ss) <- paste(rownames(ss), st, sep = "_")

#convert sample times from year to days

st_days <- round(unlist(lapply(st, function(x) years2days(x))))
names(st_days) <- rownames(ss)


