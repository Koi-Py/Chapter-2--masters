#peregrine Arjun 

library("MASS")
library("fitdistrplus")
library("dplyr")

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

##load in data of population during years
peregrine <- read.csv("Peregrine Arjun-grouse moor nests.csv")

meanterri <- mean(peregrine$number.of.occupied.territories)
sd <- sd(peregrine$number.of.occupied.territories)

no.persecmean <- (diffnon/100) * meanterri
no.persecsd <- (diffnon/100) * sd

persecuted <- data.frame(no.persecmean, no.persecsd)
mean(persecuted$no.persecmean)
sd(persecuted$no.persecsd)

