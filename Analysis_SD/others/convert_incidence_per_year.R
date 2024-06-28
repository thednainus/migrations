model1 <- readRDS(system.file("data/ECDC_incidence_model_22Oct2021.RDS", package = "HIVepisimAnalysis"))
names(model1) <- c("Year", "N_Inf_M")

model1["decimal_only"] <- model1$N_Inf_M - floor(model1$N_Inf_M)
model1["integer_only"] <- model1$N_Inf_M - model1$decimal_only

#convert value per year to a random value per weeks
#1 year = 52 weeks
#generates 52 random numbers

year_decimal <- seq( 1980, 2020, length.out = 124)

gerate_random_numbers <- function(n = 3, max_value){

  rand_numbers <- floor(runif(n, min = 0, max = max_value))
  sort_numbers <- sort(rand_numbers)
  n1 <- c(sort_numbers, max_value)
  n2 <- c(0, sort_numbers)

  all_numbers <- data.frame(n1 = n1, n2 = n2)
  all_numbers["subtraction"] <- all_numbers$n1 - all_numbers$n2


  return(sort(all_numbers$subtraction))
}

part1 <- gerate_random_numbers(n= 15, max_value = sum(model1$integer_only[1:5]))
part2 <- gerate_random_numbers(n= 36, max_value = sum(model1$integer_only[6:19]))
part2 <- sort(part2, decreasing = TRUE)
part3 <- gerate_random_numbers(n= 27, max_value = sum(model1$integer_only[20:27]))
part4 <- gerate_random_numbers(n= 42, max_value = sum(model1$integer_only[28:41]))
part4 <- sort(part4, decreasing = TRUE)

parts <- c(part1, part2, part3, part4)

incidence_weeks <- unlist(lapply(model1$integer_only, function(x) gerate_random_numbers(max_value = x)))


incidence_teste <- data.frame(year = year_decimal, incidence = parts)
