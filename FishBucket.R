FishPopulation = sample(1:1000,1)
prior = sample(1:FishPopulation,1)

# Initialize a data frame to store the results of each trip
bucket <- data.frame(
  "day" = 1:21,
  "n_caught" = c(sample(1:FishPopulation,1),rep(NA,20)),
  "n_marked" = c(0,rep(NA,20)),
  "hypoth" = c(prior,rep(NA,20)),
  "posterior" = matrix(rep(NA,21*FishPopulation), ncol=FishPopulation)
)

# Iterate through each trip and update the posterior distribution
for (i in 2:21) {
  # Simulate catching some fish and marking them
  n_caught = sample(1:FishPopulation,1)
  n_marked = sample(1:n_caught,1)
  
  # Compute the likelihood of the data given each possible hypothesis
  
  
  log_pmf <- function(k, n, p) {
    lp <- lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1) + k * log(p) + (n - k) * log(1 - p)
    return(lp)
  }
  
  likelihood <- exp(log_pmf(n_marked, n_caught, 1:FishPopulation/1000))
  
  
  
  # Compute the evidence by summing the likelihoods over all possible hypotheses
  evidence = sum(likelihood * dbeta(bucket$hypoth[1:i], 1, 1))
  
  # Compute the posterior distribution by applying Bayes' rule
  posterior = likelihood * dgamma(bucket$hypoth[1:i], 1, 1) / evidence
  
  # Update the data frame with the new results
  bucket$n_caught[i] = n_caught
  bucket$n_marked[i] = n_marked
  bucket$posterior[i,] = posterior
  
  # Sample a new hypothetical population size for the next iteration
  bucket$hypoth[i+1] = sample(1:FishPopulation,1,prob=posterior)
}

