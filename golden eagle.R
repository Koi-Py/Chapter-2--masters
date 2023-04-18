#Golden Eagle

rm(list=ls())

#persecution rate

n = 131
tracking = 46
natural = 9
dropped_not_sus = 18
malfunction = 6
bat_died = 7
#persecution
dropped_sus = 1
killed = 5
stop_no_mal = 38

#set prob
prob = (dropped_sus+killed+stop_no_mal)/131
n = 131

sample <- rbinom(n = 1000, size = n, prob = prob)
data.frame(sample)

#population data

n_breeding_pairs <- 508

n_product <- c(1.29, 1.14, 1.30, 1.56, 1, 1, 0.94, 1.2) 
mean_product <- mean(n_product)
sd_n_product <- sd(n_product)

#make data frame
n_rows <- length(sample)
my_mat <- matrix(rep(n_breeding_pairs, each = n_rows))

product <- matrix(rep(mean_product, each = n_rows))

productsd <- matrix(rep(sd_n_product, each = n_rows))


data <- data.frame(sample,
                   my_mat,
                   product,
                   productsd)

data$first_years <- data$my_mat * data$product
data$first_years_sd <- data$my_mat * data$productsd
data$sample_percen <- data$sample/100

data$persecution <- data$sample_percen * data$first_years
data$persecution_sd <- data$sample_percen * data$first_years_sd
mean(data$persecution)
sd(data$persecution_sd)

hist(data$persecution)
