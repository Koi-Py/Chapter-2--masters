#hen harrier meg

#simualte the probability test runs 

#no.years = first year of birds

first_perse <- rbinom(n = 1, size = 120, prob = 0.6192) #estimating for the 17 years
hist(first_perse)
first_perse

df <- data.frame()
n_iterations <- 1000

for (i in 1:n_iterations){
  # Random sample simulation
  sample <- rbinom(n = 1, size = 13, prob = 0.6192) #size <- no. firstyrs
  
  # Create data frame
  df <- rbind(df, sample)
}


#simulate for scotlands upper and lower bound size or use the average, 

# Population estimates

england_2010 = 12
england_2016 = 4
mean_england <-mean(c(england_2010, england_2016))
sd_england <- sd(c(england_2010, england_2016))

# productivity 2002-2008
eng_product <- 1.57
sd_product <- 0.51

# first year birds 
england_firstyr <- mean_england * eng_product
england_firstyr_sd <- sd_england * sd_product


se_difference <- england_firstyr_sd

# Calculate the 95% confidence interval
lower_ci <- england_firstyr - 1.96 * se_difference
upper_ci <- england_firstyr + 1.96 * se_difference

# Print the results
cat("Mean of difference: ", england_firstyr, "\n")
cat("95% CI of difference: [", lower_ci, ", ", upper_ci, "]\n")

# how do you carry through the variability in size/number of trials


#scotland
