FishPopulation = sample(1:1000,1)
prior = sample(1:FishPopulation,1)

# Initialize a data frame to store the results of each trip
bucket <- data.frame(
  "day" = 1:101,
  "n_caught" = c(sample(1:FishPopulation,1),rep(NA,100)),
  "n_marked" = c(0,rep(NA,100)),
  "hypoth" = c(prior,rep(NA,100)),
  "posterior" = matrix(rep(NA,101*FishPopulation), ncol=FishPopulation)
)






###############################################################################################

i=2


log_pmf <- function(k, n, p) {
  lp <- lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1) + k * log(p) + (n - k) * log(1 - p)
  return(lp)
}
n_caught = sample(1:FishPopulation,1)
n_marked = sample(1:n_caught,1)

likelihood <- exp(log_pmf(bucket$n_marked[i-1], bucket$n_caught[i-1], 1:FishPopulation/1000))
evidence = sum(likelihood * dgamma(bucket$hypoth[1:i-1], 1, 1))
# Compute the posterior distribution by applying Bayes' rule
posterior = likelihood * dgamma(bucket$hypoth[1:i-1], 1, 1) / evidence

# Update the data frame with the new results
bucket$n_caught[i] = n_caught
bucket$n_marked[i] = n_marked

for(j in 1:FishPopulation){
  
  bucket[i-1,4+j]<-posterior[j]
  
}


# Sample a new hypothetical population size for the next iteration
bucket$hypoth[i] = sample(1:FishPopulation,1,prob=posterior)

i = i+1

