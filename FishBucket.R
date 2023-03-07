#RScript to explain Bayes Posterior Probability and updating

# So we have a fisherman and a lake with an undetermined number of fish in it.

# The fisherman casts a net and brings in say 20 fish, and marks all of them.

# The next day the fisherman casts a net and reels is another 20 fish, 4 of which have marks from the previous day.


# We need to assign the probabilities of the events.

#P(H|E): is the probability that the number of fish in the lake (H) given the evidence. H is the value of the first guess in this case.




N <- 100
prior <- rep(1/N, N)
evidence <- sum(prior * likelihood(1:N, 5, 40))
posterior <- likelihood(1:N, 40, 40) * prior / evidence
plot(posterior, type="h")

N <- 1000
prior <- rep(1/N, N)
evidence <- sum(prior * likelihood(1:N, 40, 40))
posterior <- likelihood(1:N, 40, 40) * prior / evidence
plot(posterior, type="h")
