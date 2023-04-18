#peregrine Arjun 

library("MASS")
library("fitdistrplus")

setwd("C:/Users/Koi/OneDrive - University of Cape Town/Documents/Masters/Chapter 2/getting started")

peregrine <- read.csv("Peregrine Arjun-grouse moor nests.csv")
x <- peregrine$number.of.occupied.territories 
plot(peregrine)

# Set probability and standard deviation of nongrouse Moors 
prob <- 0.67
sd <- 0.04 

# Calculate sample size
size <- (prob*(1-prob)) / (sd^2)

int_size <- as.integer(size)

# Generate random sample from binomial distribution
sample <- rbinom(n = 1000, size = int_size, prob = prob)
hist(sample, breaks = 20, main = "Binomial Distribution")

Data <- data.frame(
  sample_nongrouse = sample
)

# Set probability and standard deviation of grouse Moors 
gprob <- 0.33
gsd <- 0.05 

# Calculate sample size
gsize <- (gprob*(1-gprob)) / (gsd^2)

gint_size <- as.integer(gsize)

# Generate random sample from binomial distribution
gsample <- rbinom(n = 1000, size = gint_size, prob = prob)
hist(sample, breaks = 20, main = "Binomial Distribution")

Data <- data.frame(
  sample_grouse = sample , 
  sample_nongrouse = gsample
)

Data$diff <-  Data$sample_grouse - Data$sample_nongrouse
hist(Data$diff)

diffnon <- Data$diff[Data$diff >= 0]
summary(diffnon)
length(diffnon)
hist(diffnon)



#set sample size at 

# Set probability and standard deviation of nongrouse Moors 
prob <- 0.67
sd <- 0.04 


# Generate random sample from binomial distribution
sample <- rbinom(n = 1000, size = 947, prob = prob)
hist(sample, breaks = 20, main = "Binomial Distribution")

Data <- data.frame(
  sample_nongrouse = sample
)

# Set probability and standard deviation of grouse Moors 
gprob <- 0.33
gsd <- 0.05 

# Generate random sample from binomial distribution
gsample <- rbinom(n = 1000, size = 947, prob = prob)
hist(sample, breaks = 20, main = "Binomial Distribution")

Data <- data.frame(
  sample_grouse = sample , 
  sample_nongrouse = gsample
)

##diff percentage
Data$diff <- Data$sample_nongrouse - Data$sample_grouse
hist(Data$diff)

#summary
diffnon <- Data$diff[Data$diff >= 0]
summary(diffnon)
length(diffnon)
hist(diffnon)

setwd("C:/Users/DELL/Documents/masters/Chapter 2/R")
##load in data of population during years
tperegrine <-read.csv("peregrine transposed.csv")

#fill values in a data frame
n <- length(diffnon)
nyear <- length(tperegrine)

# replicate the vector rows n times
my_mat <- matrix(rep(tperegrine, each = n), ncol = nyear)
my_matcol <- colnames(tperegrine)
colnames(my_mat) <- c(my_matcol)

#add to df 
year_df <- data.frame(diffnon, my_mat)

#multiply success diff by territories
year_df$diffnon <- as.numeric(year_df$diffnon)
year_df$diffnon <- year_df$diffnon/100
summary(year_df)
year_df$X1992 <- as.numeric(year_df$X1992)
year_df$X1993 <- as.numeric(year_df$X1993)
year_df$X1994 <- as.numeric(year_df$X1994)
year_df$X1995 <- as.numeric(year_df$X1995)
year_df$X1996 <- as.numeric(year_df$X1996)
year_df$X1997 <- as.numeric(year_df$X1997)
year_df$X1998 <- as.numeric(year_df$X1998)
year_df$X1999 <- as.numeric(year_df$X1999)
year_df$X2000 <- as.numeric(year_df$X2000)
year_df$X2002 <- as.numeric(year_df$X2002)
year_df$X2003 <- as.numeric(year_df$X2003)
year_df$X2004 <- as.numeric(year_df$X2004)
year_df$X2005 <- as.numeric(year_df$X2005)
year_df$X2006 <- as.numeric(year_df$X2006)

year_df$X1992success <- year_df$X1992 * year_df$diffnon
year_df$X1993success <- year_df$X1993 * year_df$diffnon
year_df$X1994success <- year_df$X1994 * year_df$diffnon
year_df$X1995success <- year_df$X1995 * year_df$diffnon
year_df$X1996success <- year_df$X1996 * year_df$diffnon
year_df$X1997success <- year_df$X1997 * year_df$diffnon
year_df$X1998success <- year_df$X1998 * year_df$diffnon
year_df$X1999success <- year_df$X1999 * year_df$diffnon
year_df$X2000success <- year_df$X2000 * year_df$diffnon
year_df$X2002success <- year_df$X2002 * year_df$diffnon
year_df$X2003success <- year_df$X2003 * year_df$diffnon
year_df$X2004success <- year_df$X2004 * year_df$diffnon
year_df$X2005success <- year_df$X2005 * year_df$diffnon
year_df$X2006success <- year_df$X2006 * year_df$diffnon

success <- select(year_df, (ncol(year_df)-13):ncol(year_df))

successint <- lapply(success,as.integer)
successint <- data.frame(successint)