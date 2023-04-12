set.seed(98)
library(tidyverse)
i=2
j = 20

# Set the initial values
FishPopulation <- sample(20:1000,1)
prior <- dbinom(20:1000,size = 1, prob = 1/FishPopulation)


# Initialize a data frame to store the results of each day
bucket <- data.frame(
  "day" = 1:31,
  "probability_draw_marked" = c(0,rep(NA,30)),
  "n_marked_c" = c(0,rep(NA,30)),
  "total_marked_r" = c(20,rep(NA,30)),
  "hypoth" = c(300,rep(NA,30))
)

# Initialize the posterior matrix
posterior_matrix <- matrix(0, nrow = length(20:FishPopulation), ncol = 31)

posterior_matrix[,1]<- prior

# Iterate through each day and update the posterior distribution
for (i in 2:30) {
  # Simulate catching some fish and marking them
  bucket$probability_draw_marked[i] <- bucket$total_marked_r[i-1]/FishPopulation
  bucket$n_marked_c[i] <-rbinom(n = 1,size = 20, prob = bucket$probability_draw_marked[i])
  bucket$total_marked_r[i]<-bucket$total_marked_r[i-1]+bucket$n_marked_c[i]+(20 - bucket$n_marked_c[i])
  
  # Compute the likelihood of the data given each possible hypothesis
  for( j in 1:FishPopulation){
    p_marked <- bucket$total_marked_r[i-1]/j
    likelihood <- dbinom(bucket$n_marked_c[i],size = 20, prob = p_marked)
    posterior_matrix[j,i]<-likelihood
  }
  
  # Compute the prior by taking the previous posterior as the new prior
  prior <- posterior_matrix[,i-1]
  
  # Compute the unnormalized posterior by multiplying the likelihood and prior
  unnormalized_posterior <- likelihood * prior
  
  # Compute the normalized posterior
  posterior <- unnormalized_posterior / sum(unnormalized_posterior)
  
  # Check if the evidence is zero or very small
  if (sum(posterior > 0) == 0) {
    message("Evidence is zero or very small at day ", i, ". Aborting.")
    break
  }
  
  # Update the data frame with the new results
  bucket$hypoth[i] <- sample(1:FishPopulation, 1, replace = TRUE, prob = posterior)
  
  # Update the posterior matrix with the new results
  posterior_matrix[,i] <- posterior
}

# Check if the loop was completed
if (i == 21) {
  message("The loop completed successfully.")
} else {
  message("The loop was aborted at day ", i, ".")
}

# Compute the maximum a posteriori (MAP) estimate for each day
MAP_estimate <- apply(posterior_matrix, 2, function(x) which.max(x))

# Plot the posterior distribution for each day
for (i in 1:21) {
  ggplot(data = data.frame(x = 1:FishPopulation, y = posterior_matrix[,i]), aes(x = x, y = y)) +
    geom_line() +
    geom_vline(xintercept = MAP_estimate[i], linetype = "dashed", color = "red") +
    labs(title = paste("Day", i), x = "Population size", y ="Posterior Probability")
         