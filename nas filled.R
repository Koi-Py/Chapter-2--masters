
# generate random vector for the single column
RandomNum <- runif(519, 1, 99)
#and the df for the 
df_x = as.data.frame(RandomNum)
#convert to  vector so easy to add to colum
df_x


#let say we have 20 years 

years <- runif(20, 1990, 2023)

year_values <- runif(20, 1, 99)



# replicate the vector rows n times
n <- 519
my_mat <- matrix(rep(year_values, each = n), ncol = 20)

#add to df 
year_df <- data.frame(my_mat)

#have to figure out how to change the heading names

colnames(year_df) <- colnames(years)


rownames(year_df) <- df_x


