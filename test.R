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