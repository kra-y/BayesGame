set.seed(98)
library(tidyverse)

# Set the initial values
FishPopulation <- sample(20:1000, 1)
prior <- rep(1/FishPopulation, FishPopulation)

# Initialize a data frame to store the results of each day
bucket <- data.frame(
  "day" = 1:21,
  "n_caught" = rep(20,21),
  "n_marked" = rep(NA,21),
  "n_umarked" = c(FishPopulation,rep(NA,20)),
  "total_unmarked" = rep(NA,21),
  "total_marked_fish" = rep(NA,21),
  "probability_draw_marked" = c(0,rep(NA,20)),
  "hypoth" = c(300,rep(NA,20))
)

# Initialize the posterior matrix
posterior_matrix <- matrix(0, nrow = FishPopulation, ncol = 21)

# Iterate through each day and update the posterior distribution
for (i in 1:21) {
  # Simulate catching some fish and marking them
  if (i == 1) {
    n_caught <- 20
    n_marked <- 0
    n_unmarked<-20
  } else {
    n_caught <- 20
    n_unmarked <- 
    n_marked <- 
  }
  
  # Store the number of marked fish
  bucket$n_marked[i] <- n_marked
  
  # Compute the likelihood of the data given each possible hypothesis
  likelihood <- dbinom(n_marked, n_caught, 1:FishPopulation/FishPopulation)
  
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
         