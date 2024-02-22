v1 <- read.csv2("splines_phydyn_beast2/sampleFromPrior/beast_phydyn_splines.log", sep = "\t", skip = 151)
head(v1)
class(v1$ucldMean)
v1$ucldMean <- as.numeric(v1$ucldMean)
head(v1[v1$ucldMean >1,])
max(v1$ucldMean)

v1.2 <- read.csv2("splines_phydyn_beast2/sampleFromPrior/run2/beast_phydyn_splines.log", sep = "\t", skip = 151)
head(v1.2)
class(v1.2$ucldMean)
v1.2$ucldMean <- as.numeric(v1.2$ucldMean)
head(v1.2[v1.2$ucldMean >1,])
max(v1$ucldMean)



v2 <- read.csv2("splines_phydyn_beast2/sampleFromPrior_2/beast_phydyn_splines.log", sep = "\t", skip = 151)
head(v2)
class(v2$ucldMean)
v2$ucldMean <- as.numeric(v2$ucldMean)
plot(hist(v2$ucldMean))
v1$ucldMean <- as.numeric(v1$ucldMean)


v3 <- read.csv2("splines_phydyn_beast2/sampleFromPrior_3/beast_phydyn_splines.log", sep = "\t", skip = 169)
head(v3)
class(v3$ucldMean)
v3$ucldMean <- as.numeric(v3$ucldMean)
plot(hist(v3$ucldMean))

v4 <- read.csv2("splines_phydyn_beast2/sampleFromPrior_4/beast_phydyn_splines.log", sep = "\t", skip = 169)
head(v4)
class(v4$ucldMean)
v4$ucldMean <- as.numeric(v4$ucldMean)
plot(hist(v4$ucldMean))


v5 <- read.csv2("splines_phydyn_beast2/sampleFromPrior_5/beast_phydyn_splines.log", sep = "\t", skip = 151)
head(v5)
class(v5$ucldMean)
v5$ucldMean <- as.numeric(v5$ucldMean)
plot(hist(v5$ucldMean))
max(v5$ucldMean)
min(v5$ucldMean)

v5.data <- read.csv2("splines_phydyn_beast2/sampleFromPrior_5/with_data/beast_phydyn_splines.log", sep = "\t", skip = 169)
head(v5.data)
class(v5.data$ucldMean)
v5.data$ucldMean <- as.numeric(v5.data$ucldMean)
plot(hist(v5.data$ucldMean))
max(v5.data$ucldMean)
min(v5.data$ucldMean)


