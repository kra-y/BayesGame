library(dplyr)
library(ggplot2)
FishPopulation = sample(1:1000,1)
prior = sample(1:FishPopulation,1)
numNA = FishPopulation-prior


prior_plot_data <- data.frame("Population" = seq(1, FishPopulation),
                              "Prior" = c(rep(1, prior), rep(0, numNA)),
                              "P_of_Pop" = rep(1/FishPopulation, FishPopulation))

prior_plot_data %>%
  ggplot(aes(x = Population, y = P_of_Pop, fill = factor(Prior))) +
  geom_histogram(stat = "identity", binwidth = 1, color = NA) +
  scale_fill_manual(values = c("red", "gray")) +
  labs(fill = "Prior") +
  theme_classic()
