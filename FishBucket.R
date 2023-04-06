set.seed(98)
# Set the initial values
FishPopulation <- sample(20:1000, 1)
prior <- sample(20:FishPopulation, 1)

# Initialize a data frame to store the results of each trip
bucket <- data.frame(
  "day" = 1:210,
  "n_caught" = rep(20,210),
  "n_marked" = c(0,rep(NA,20)),
  "hypoth" = c(prior,rep(NA,20))
)

# Initialize the posterior matrix
posterior_matrix <- matrix(0, nrow = 981, ncol = 210)

# Iterate through each trip and update the posterior distribution
for (i in 2:210) {
  # Simulate catching some fish and marking them
  n_caught <- 20
  n_marked <- sample(1:n_caught, 1)
  
  # Compute the log-likelihood of the data given each possible hypothesis
  log_likelihood <- dbinom(n_marked, n_caught, 20:1000/1000, log = TRUE)
  
  # Compute the log-prior by taking the log of the gamma distribution
  log_prior <- dgamma(bucket$hypoth[i-1], 1, 1, log = TRUE)
  
  # Compute the log-posterior by applying Bayes' rule using the log-sum-exp trick
  log_posterior <- log_likelihood + log_prior
  log_posterior <- log_posterior - max(log_posterior)
  posterior <- exp(log_posterior) / sum(exp(log_posterior))
  
  # Check if the evidence is zero or very small
  if (sum(posterior > 0) == 0) {
    message("Evidence is zero or very small at iteration ", i, ". Aborting.")
    break
  }
  
  # Update the data frame with the new results
  bucket$n_marked[i] <- n_marked
  
  # Update the posterior matrix with the new results
  posterior_matrix[,i] <- posterior
  
  # Sample a new hypothetical population size for the next iteration
  bucket$hypoth[i] <- sample.int(20:1000, 1, replace = TRUE, prob = posterior)
}

# Check if the loop was completed
if (i == 210) {
  message("The loop completed successfully.")
} else {
  message("The loop was aborted at iteration ", i, ".")
}
