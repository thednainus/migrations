# defining upper and lower values for priors

a1980 <- rnorm(50, 0, 0.001)
b1980 <- rnorm(100, 0, 2)
#b1980 <- rlnorm(100, 0, 0.5)
#b1980 <- rnorm(50, 0, 0.5)
time <- seq(1980, 1994, by = 0.5)

total_linear_fun <- function(t, a1980, b1980){

  value <- a1980*t + b1980

  all_data <- data.frame(time = t, a = a1980, b = b1980, value = value)

  return(all_data)
}


all_data <- data.frame()
for( t in time){

  for (b in b1980){

    for (a in a1980)

      total_linear <- total_linear_fun(t,a,b)
    all_data <- rbind(all_data, total_linear)

  }


}
all_data["a_times_t"] <- all_data$time * all_data$a
all_data["maxb2"] <- 2 - all_data$b
all_data["beta1980"] <- unlist(lapply(all_data$value, function(x) max(0.0, x)))
all_data["lt2"] <- all_data$value[all_data$value > 0 & all_data$value <= 2]
wrong_values <- subset(all_data, beta1980 == 0)



a1980 <- rnorm(25, 0, 40)
b1980 <- rnorm(25, 0, 50)
a1995 <- rnorm(10, 0, 40)
time <- seq(1995, 2000, by = 0.5)

all_data <- data.frame()
total_linear_fun2 <- function(t, a1980, b1980, a1995){

  value <- b1980+(a1980-a1995)*1995.0+a1995*t

  all_data <- data.frame(time = t, a1980 = a1980, b1980 = b1980, a1995 = a1995, value = value)

  return(all_data)
}

all_data <- data.frame()
for( t in time){

  for (a in a1980){

    for (b in b1980){

      for (c in a1995){
        total_linear <- total_linear_fun2(t,a,b,c)
        all_data <- rbind(all_data, total_linear)

      }
    }
  }
}

all_data["beta1995"] <- unlist(lapply(all_data$value, function(x) max(0.0, x)))
wrong_values <- subset(all_data, beta1995 == 0)



a1980 <- rnorm(25, 0, 40)
b1980 <- rlnorm(25, 4.6, 1)
a1995 <- rnorm(25, 0, 40)
b1995 <- rlnorm(25, 4.6, 1)
a2005 <- rnorm(25, 0, 40)

time <- seq(2005, 2020, by = 0.1)

all_data <- data.frame()
total_linear_fun3 <- function(t, a1980, b1980, a1995, a2005){

  value <- (2005.0*(a1995-a2005))+b1980+(1995.0*(a1980-a1995))+a2005*t

  all_data <- data.frame(time = t, a1980 = a1980, b1980 = b1980, a1995 = a1995,
                         a2005 = a2005, value = value)

  return(all_data)
}

all_data <- data.frame()
for( t in time){

  for (a in a1980){

    for (b in b1980){

      for (c in a1995){
        total_linear <- total_linear_fun2(t,a,b,c)
        all_data <- rbind(all_data, total_linear)

      }
    }
  }
}

all_data["beta1980"] <- unlist(lapply(all_data$value, function(x) max(0.0, x)))
wrong_values <- subset(all_data, beta1980 == 0)



#for the rate if starting ART

a <- rnorm(50, 0, 0.0001)
b <- rnorm(100, 0, 1)

time <- seq(1995, 2020, by = 0.5)

total_linear_fun <- function(t, a, b){

  value <- a*t + b

  all_data <- data.frame(time = t, a = a, b = b, value = value)

  return(all_data)
}


all_data <- data.frame()
for( t in time){

  for (i in b){

    for (j in a)

      total_linear <- total_linear_fun(t,j,i)
      all_data <- rbind(all_data, total_linear)

  }


}
all_data["a_times_t"] <- all_data$time * all_data$a
all_data["maxb2"] <- 2 - all_data$b
all_data["beta1980"] <- unlist(lapply(all_data$value, function(x) max(0.0, x)))
all_data["lt2"] <- all_data$value[all_data$value > 0 & all_data$value <= 2]
wrong_values <- subset(all_data, beta1980 == 0)
