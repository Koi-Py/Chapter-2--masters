#peregrine Arjun 

library("MASS")
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


# Generate random sample from binomial distribution
sample <- rbinom(n = 1000, size = 22, prob = prob)
hist(sample, breaks = 20, main = "Binomial Distribution")

Data <- data.frame(
  sample_nongrouse = sample
)

# Set probability and standard deviation of grouse Moors 
gprob <- 0.33

# Generate random sample from binomial distribution
gsample <- rbinom(n = 1000, size = 22, prob = prob)
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

####
difference <- rbinom(n = 17, size = 22, prob = 0.34) #estimating for the 17 years
hist(difference)
difference #no. persecuted nests for each year

# Calculate sum of the difference dataframe
sum_difference <- sum(difference)

# Print the result
cat("Sum of difference: ", sum_difference, "\n")
####


results <- list()

# Set the number of iterations to 100
n_iterations <- 100

# Loop over the number of iterations
for (i in 1:n_iterations) {
  # Generate a random sample using rbinom()
  difference <- rbinom(n = 17, size = 22, prob = 0.34)
  
  # Append the results to the list
  results[[i]] <- difference
}

# Print the first 4 elements of the list
head(results, n = 4)


# Calculate mean of the difference dataframe
mean_difference <- mean(difference)

# Calculate standard error of the mean
se_difference <- sqrt(var(difference) / length(difference))

# Calculate the 95% confidence interval
lower_ci <- mean_difference - 1.96 * se_difference
upper_ci <- mean_difference + 1.96 * se_difference

# Print the results
cat("Mean of difference: ", mean_difference, "\n")
cat("95% CI of difference: [", lower_ci, ", ", upper_ci, "]\n")


##load in data of population during years
peregrine <- read.csv("Peregrine Arjun-grouse moor nests.csv")

meanterri <- mean(peregrine$number.of.occupied.territories)
sd <- sd(peregrine$number.of.occupied.territories)

no.persecmean <- (difference/100) * 22.07
no.persecsd <- (difference/100) * sd

persecuted <- data.frame(no.persecmean, no.persecsd)
mean(persecuted$no.persecmean)
sd(persecuted$no.persecsd)

mean(no.persecmean)


