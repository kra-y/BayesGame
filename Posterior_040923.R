set.seed(98)
library(tidyverse)


#the game will necessarily need to have the player select the size of the fisherman's bucket, the lower estimate
# and the upper estimate for the prediction, and the limit for size of lake.


# Set the initial values
FishPopulation <- sample(1000:10000, 1)
# we need to set the initial prior to reflect our hypothesis. In our scenario, let's assume the fisherman sees the lake and
# estimates by marking the first 20 fis and releasing them so the population is definitely at least 20.


# Fisherman's Fisrt Hypothesis:

# There are at least 20 fish in the lake and no more than 10000

          # H0:(19<N(fish)<=10000)


#setting the first 19 values of our prior to 0 to reflect known info
prior <- rep(0,10000)
# then set the prior to reflect the hypothesis giving an equal probability of each value between 20 and 10000

prior[20:10000]<-rep(1/(length(20:10000)),length(20:10000))
# Initialize a data frame to store the results of each day
bucket <- data.frame(
  "day" = 1:365,
  "probability_draw_marked" = c(0,rep(NA,364)),
  "n_marked_c" = c(0,rep(NA,364)),
  "total_marked_r" = c(20,rep(NA,364))
)

# Initialize the posterior matrix
posterior_matrix <- matrix(0, nrow = length(prior), ncol = 365)
posterior_matrix[,1]<- prior

# Iterate through each day and update the posterior distribution
for (i in 2:365) {
  # Simulate catching some fish and marking them
  bucket$probability_draw_marked[i] <- bucket$total_marked_r[i-1]/FishPopulation
  bucket$n_marked_c[i] <-rbinom(n = 1,size = 20, prob = bucket$probability_draw_marked[i])
  bucket$total_marked_r[i]<-bucket$total_marked_r[i-1]+(20 - bucket$n_marked_c[i])
  
  # Compute the likelihood of the data given each possible hypothesized population size
  for (j in 1:length(prior)) {
    if (j < bucket$total_marked_r[i]) { #if the estimate is less than the total number of marked fish in the lake,
      # set the likelihood of that estimate to 0.
      posterior_matrix[j, i] <- 0
      
    } else {
      p_marked <- bucket$total_marked_r[i-1]/j #calculate the probability of drawing a marked fish 
      likelihood <- dbinom(bucket$n_marked_c[i],size = 20, prob = p_marked)
      posterior_matrix[j, i] <- likelihood*prior[j]
    }
  }
  # Compute the posterior distribution
  posterior <- apply(posterior_matrix[, 1:i], 2, sum)
  posterior <- posterior/sum(posterior)
  
  # get the index of the maximum value of the likelihood object
  

  
  # find the .05 and .95 values of the posterior
  
  q_05<- quantile(posterior[posterior!=0],.05)
  
  q_95<-quantile(posterior[posterior!=0],.95)
  
  # find the indexes of the 05 and .95 values
  
  idx_05 <- which(posterior >= q_05)[1]
  
  idx_95 <- which(posterior >= q_95)[1] 
  
  # update prior for next iteration
  prior<-rep(0,10000)
  prior[idx_05:idx_95]<-rep(1/length(idx_05:idx_95),idx_95 - idx_05+1)
}
  
