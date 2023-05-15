
# Load ggplot2 library
library(ggplot2)

# Peregrine data frame (example)
peregrine <- data.frame(year = c(1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005, 2006),
                        number.of.occupied.territories = c(23, 22, 29, 23, 23, 23, 27, 20, 21, 19, 17, 17, 24, 21))

#### total over all years####

# Parameters
iterations <- 1000
prob <- 0.34

# Initialize a data frame to store the results
results <- data.frame(iteration = integer(0),
                      mean = numeric(0),
                      ci_lower = numeric(0),
                      ci_upper = numeric(0),
                      stringsAsFactors = FALSE)

# Iterate over each iteration
for (i in 1:iterations) {
  # Simulate persecuted nests for all years in a single binomial draw
  persecuted_nests <- sapply(peregrine$number.of.occupied.territories, rbinom, n = 1, prob = prob)
  
  # Calculate the sum of persecuted nests across all years
  sum_persecuted_nests <- sum(persecuted_nests)
  
  # Append the results to the data frame
  results <- rbind(results, data.frame(iteration = i,
                                       mean = sum_persecuted_nests,
                                       stringsAsFactors = FALSE))
}

# Print the results
print(results)

# Calculate the overall mean
overall_mean <- mean(results$mean)

hist(results$mean, breaks = 50)

# Calculate the 95% confidence intervals
ci_lower <- quantile(results$mean, 0.025)
ci_upper <- quantile(results$mean, 0.975)

overall_mean
ci_lower
ci_upper

# Create a ggplot2 graph
ggplot(results, aes(x = factor(iteration), y = mean)) +
  geom_point(size = 3) +
  labs(x = "Iteration", y = "Sum Persecuted Nests (All Years)") +
  theme_minimal() +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = overall_mean, linetype = "dashed", color = "red", size = 1)


#### mean in each year####

# Parameters
iterations <- 100
prob <- 0.34

# Initialize a data frame to store the results
results <- data.frame(iteration = integer(0),
                      mean = numeric(0),
                      ci_lower = numeric(0),
                      ci_upper = numeric(0),
                      stringsAsFactors = FALSE)

# Iterate over each iteration
for (i in 1:iterations) {
  # Simulate persecuted nests for all years in a single binomial draw
  persecuted_nests <- sapply(peregrine$number.of.occupied.territories, rbinom, n = 1, prob = prob)
  
  # Calculate the mean and standard error
  mean_persecuted_nests <- mean(persecuted_nests)
  se_persecuted_nests <- sd(persecuted_nests) / sqrt(length(persecuted_nests))
  
  # Calculate the 95% confidence intervals
  ci_lower <- mean_persecuted_nests - 1.96 * se_persecuted_nests
  ci_upper <- mean_persecuted_nests + 1.96 * se_persecuted_nests
  
  # Append the results to the data frame
  results <- rbind(results, data.frame(iteration = i,
                                       mean = mean_persecuted_nests,
                                       ci_lower = ci_lower,
                                       ci_upper = ci_upper,
                                       stringsAsFactors = FALSE))
}

# Print the results
print(results)

# Load ggplot2 library
library(ggplot2)

# Create a ggplot2 graph
ggplot(results, aes(x = factor(iteration), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Iteration", y = "Mean Persecuted Nests (All Years)") +
  theme_minimal() +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the overall mean
overall_mean <- mean(results$mean)

# Calculate the 95% confidence intervals
ci_lower <- quantile(results$mean, 0.025)
ci_upper <- quantile(results$mean, 0.975)

overall_mean
ci_lower
ci_upper

hist(results$mean)

# Create a ggplot2 graph
ggplot(results, aes(x = factor(iteration), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Iteration", y = "Mean Persecuted Nests (All Years)") +
  theme_minimal() +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = overall_mean, linetype = "dashed", color = "red", size = 1)




#### hen harrier just in england####


# hen harrier data frame - but is still called peregrine!
peregrine <- data.frame(year = c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                        nest_attempts = c(19, 22, 23, 19, 12, 12, 9, 1, 2, 4, 13, 3, 7, 14),
                        successful_attempts = c(15, 12, 14, 10, 6, 7, 4, 1, 0, 4, 6, 3, 3, 9),
                        est_young = c(30, 35, 36, 30, 19, 19, 14, 2, 3, 6, 20, 5, 11, 22))

# Parameters
iterations <- 100
prob <- 0.6192

# Initialize a data frame to store the results
results <- data.frame(iteration = integer(0),
                      mean = numeric(0),
                      ci_lower = numeric(0),
                      ci_upper = numeric(0),
                      stringsAsFactors = FALSE)

# Iterate over each iteration
for (i in 1:iterations) {
  # Simulate persecuted birds for all years in a single binomial draw
  persecuted_birds <- sapply(peregrine$est_young, rbinom, n = 1, prob = prob)
  
  # Calculate the mean and standard error
  mean_persecuted_birds <- mean(persecuted_birds)
  se_persecuted_birds <- sd(persecuted_birds) / sqrt(length(persecuted_birds))
  
  # Calculate the 95% confidence intervals
  ci_lower <- mean_persecuted_birds - 1.96 * se_persecuted_birds
  ci_upper <- mean_persecuted_birds + 1.96 * se_persecuted_birds
  
  # Append the results to the data frame
  results <- rbind(results, data.frame(iteration = i,
                                       mean = mean_persecuted_birds,
                                       ci_lower = ci_lower,
                                       ci_upper = ci_upper,
                                       stringsAsFactors = FALSE))
}

# Print the results
print(results)

# Calculate the overall mean
overall_mean <- mean(results$mean)

# Calculate the 95% confidence intervals
ci_lower <- quantile(results$mean, 0.025)
ci_upper <- quantile(results$mean, 0.975)

overall_mean
ci_lower
ci_upper

# Create a ggplot2 histogram
ggplot(results, aes(x = mean)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  labs(x = "Persecuted Birds (All Years)", y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14))

# Create a ggplot2 graph
ggplot(results, aes(x = factor(iteration), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Iteration", y = "Mean Persecuted Birds (All Years)") +
  theme_minimal() +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = overall_mean, linetype = "dashed", color = "red", size = 1)

# Create a ggplot2 graph
ggplot(results, aes(x = factor(iteration), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Iteration", y = "Mean Persecuted Nests (All Years)") +
  theme_minimal() +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = overall_mean, linetype = "dashed", color = "red", size = 1)


# scotland 




#etheridge 

#simulate data frame 

# Set the confidence level and interval 1988
conf_level <- 0.95
conf_interval <- c(69, 220)

# Calculate the margin of error
margin_error <- (conf_interval[2] - conf_interval[1])/2

# Estimate the population standard deviation
z_value <- qnorm(1-(1-conf_level)/2)
pop_sd <- margin_error/z_value

# Set the sample mean
sample_mean <- 140

# Generate random data from the normal distribution with estimated parameters
data_1988 <- rnorm(4, mean = sample_mean, sd = pop_sd)

# Set the confidence level and interval 1998
conf_interval <- c(222,366)

# Calculate the margin of error
margin_error <- (conf_interval[2] - conf_interval[1])/2

# Estimate the population standard deviation
z_value <- qnorm(1-(1-conf_level)/2)
pop_sd <- margin_error/z_value

# Set the sample mean
sample_mean <- 286

# Generate random data from the normal distribution with estimated parameters
data_1998 <- rnorm(6, mean = sample_mean, sd = pop_sd)

# Set the confidence level and interval 2004
conf_interval <- c(163,216)

# Calculate the margin of error
margin_error <- (conf_interval[2] - conf_interval[1])/2

# Estimate the population standard deviation
z_value <- qnorm(1-(1-conf_level)/2)
pop_sd <- margin_error/z_value

# Set the sample mean
sample_mean <- 185

# Generate random data from the normal distribution with estimated parameters
data_2004 <- rnorm(6, mean = sample_mean, sd = pop_sd)

# Set the confidence level and interval 2010
conf_interval <- c(105,253)

# Calculate the margin of error
margin_error <- (conf_interval[2] - conf_interval[1])/2

# Estimate the population standard deviation
z_value <- qnorm(1-(1-conf_level)/2)
pop_sd <- margin_error/z_value

# Set the sample mean
sample_mean <- 194

# Generate random data from the normal distribution with estimated parameters
data_2010 <- rnorm(5, mean = sample_mean, sd = pop_sd)

df <- data.frame(year = c(1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
                          2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014),
                 no.pairs = c(data_1988, data_1998, data_2004, data_2010))

print(df)
df$rounded <- sapply(df$no.pairs, round)

# Parameters
iterations <- 100
prob <- 0.402

# Initialize a data frame to store the results
results <- data.frame(iteration = integer(0),
                      mean = numeric(0),
                      ci_lower = numeric(0),
                      ci_upper = numeric(0),
                      stringsAsFactors = FALSE)

# Iterate over each iteration
for (i in 1:iterations) {
  # Simulate persecuted nests for all years in a single binomial draw
  persecuted_nests <- sapply(df$rounded, rbinom, n = 1, prob = prob)
  
  # Calculate the mean and standard error
  mean_persecuted_nests <- mean(persecuted_nests)
  se_persecuted_nests <- sd(persecuted_nests) / sqrt(length(persecuted_nests))
  
  # Calculate the 95% confidence intervals
  ci_lower <- mean_persecuted_nests - 1.96 * se_persecuted_nests
  ci_upper <- mean_persecuted_nests + 1.96 * se_persecuted_nests
  
  # Append the results to the data frame
  results <- rbind(results, data.frame(iteration = i,
                                       mean = mean_persecuted_nests,
                                       ci_lower = ci_lower,
                                       ci_upper = ci_upper,
                                       stringsAsFactors = FALSE))
}

