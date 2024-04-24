set.seed(123)
load("Hmwk.RData")

J
S

Y = 1:3
Z = 1:3

# marginal distribution of z
get_marginal_distro_z <- function(){
  marginal = c()
  for(z in 1:3){
    marginal = c(marginal, sum(J[,z]))
  }
  
  return(marginal)
}

get_marginal_z <- function(z){
  return(sum(J[,z]))
}

sample_from_Z <- function(){
  distro = get_marginal_distro_z()
  sample(length(distro), size = 1, prob = distro)
}

sample_from_Z()

# marginal distribution of y
get_marginal_distro_y <- function(y){
  return(sum(J[y,]))
}

# marginal distribution of y given z
get_conditional_distro_y <- function(y, z){
  return(J[y,z] / get_marginal_z(z))
}

# marginal distribution of z given y
get_conditional_distro_z <- function(y, z){
  return(J[y,z] / get_marginal_distro_y(y))
}

# function that verifies both properties
is_probability_distro <- function(A){
  return(verify_non_negativity(A) & verify_sum_to_one(A))
}

# function to verify is a probability distribution is non-negative
verify_non_negativity <- function(A){
  return(sum(A >= 0) == length(A)) 
}

# function to verify is a probability distribution has sum equal to 1
verify_sum_to_one <- function(J){
  return(all.equal(sum(J), 1))
}


is_probability_distro(get_conditional_distro_z(3, Z))


#############

# Generate samples
num_samples <- 10  # Number of samples to generate
sampled_indices <- sample(length(J), num_samples, replace = TRUE, prob = J)

index_to_tuple <- function(n){
  row = ((n - 1) %% 3) + 1
  col = ((n - 1) %/% 3) + 1
  
  return(c(row, col))
}

index_to_tuple(9)

sample_from_J_vector <- function(n){
  indices = sample(length(J), n, replace = TRUE, prob = J)
  return(indices)
}

n_samples = 10000000

samples = sample_from_J_vector(n_samples)

emp_mean <- hist(samples, breaks = seq(0, 9), plot = FALSE)$counts / n_samples

norm(J - emp_mean, type = "2")

########

# sampling from marginal Z
sample_from_Z <- function(){
  distro = get_marginal_distro_Z()
  sample(length(distro), size = 1, prob = distro)
}

# sampling from Y conditioned on Z
sample_from_Y_conditioned <- function(z){
  distro = get_conditional_distro_y(Y, z)
  sample(length(distro), size = 1, prob = distro)
}

sample_marginal_conditional <- function(){
  z = sample_from_Z()
  y = sample_from_Y_conditioned(z)
  return(c(y, z))
}

set.seed(42)

sample_marginal_conditional()

#########

# marginal distribution of z
get_marginal_distro_Z <- function(){
  marginal = c()
  for(z in 1:3){
    marginal = c(marginal, sum(J[,z]))
  }
  
  return(marginal)
}

# marginal of z
get_marginal_Z <- function(z){
  return(sum(J[,z]))
}

# marginal distribution of z
get_marginal_distro_Z <- function(){
  marginal = c()
  for(z in 1:3) marginal = c(marginal, sum(J[,z]))
  
  return(marginal)
}

get_marginal_distro_Z()


get_marginal_Z(3)

#################

# Load required library
library(ggplot2)

# Parameters for gamma distribution
rate <- 1002
shape <- 3

# Generate data from gamma distribution
data <- rgamma(1000, shape, rate)

# Plot the histogram
ggplot(data.frame(x=data), aes(x)) +
  geom_histogram(aes(y=..density..), bins=30, fill="lightblue", color="black", alpha=0.7) +
  stat_function(fun = dgamma, args = list(shape = shape, rate = rate), color="red", size=1) +
  labs(title="Gamma Distribution", x="Value", y="Density") +
  theme_minimal()


data = c(1, 13, 27, 43, 73, 75, 154, 196, 220, 297, 344, 610, 734, 783, 796, 845, 859, 992, 1066, 1471)

mean(data)
sd(data)

hist(data, breaks = 25)

?hist


install.packages('MCMCpack')

library('MCMCpack')

# Define parameters
alpha <- 3.007
beta <- 1002.372

# Generate sequence of x values
x <- seq(0, 3000, length.out = 1000)  # Adjust the range and length.out as needed

# Calculate density values using the dinvgamma() function
density <- dinvgamma(x, alpha, beta)

# Plot the inverse gamma distribution
plot(x, density, type = "l", main = "Inverse Gamma Distribution", xlab = "x", ylab = "Density")


# Given mean and variance
mean_val <- 499.438
variance_val <- 247704.352

# Calculate rate parameter
rate_parameter <- 1 / mean_val

# Generate random numbers from exponential distribution
data <- rexp(1000, rate = rate_parameter)

?rexp

# Plot the histogram
hist(data, freq = FALSE, main = "Exponential Distribution",
     xlab = "Value", ylab = "Density", col = "skyblue")

# Add theoretical density curve
curve(dexp(x, rate = rate_parameter), col = "red", lwd = 2, add = TRUE)

data = c(1, 13, 27, 43, 73, 75, 154, 196, 220, 297, 344, 610, 734, 783, 796, 845, 859, 992, 1066, 1471)

mean(data)