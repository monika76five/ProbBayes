
library(tidyverse)
library(TeachBayes)

# Section 5.2
# the uniform distribution

spins <- runif(50, min = 0, max = 50)

# Section 5.3
# simulating waiting times

wait_monday <- runif(1000, min = 0, max = 10)
wait_wednesday <- runif(1000, min = 0, max = 10)
wait_friday <- runif(1000, min = 0, max = 10)
longest_wait <- pmax(wait_monday,
                     wait_wednesday,
                     wait_friday)

# Section 5.4
# computing probabilities

mean(longest_wait > 8)
mean(longest_wait > 6 & longest_wait < 10)

# Section 5.5
# summaries

mean(longest_wait)
sd(longest_wait)
quantile(longest_wait, c(0.1, 0.9))

# Section 5.6
# normal calculations

m <- 274; s <- 43
normal_area(100, 240, c(m, s),
            text = FALSE) + ggtitle("") +
            xlab("Time (Minutes)")

pnorm(140, 274, 43)

normal_area(230, 280, c(m, s),
            text = FALSE) + ggtitle("")+
            xlab("Time (Minutes)")

pnorm(280, 274, 43) - pnorm(230, 274, 43)

normal_area(300, 400, c(m, s),
            text = FALSE) + ggtitle("") +
            xlab("Time (Minutes)")

1 - pnorm(300, 274, 43)

normal_quantile(.25, c(m, s),
            text = FALSE) + ggtitle("") +
            xlab("Time (Minutes)")

qnorm(0.25, 274, 43)

normal_quantile(.90, c(m, s),
            text = FALSE) + ggtitle("") +
            xlab("Time (Minutes)")

qnorm(0.90, 274, 43)

# Section 5.7
# binomial calculations and normal curve

n <- 100
p <- 0.1

mu <- n * p
sigma <- sqrt(n * p * (1 - p))

pnorm(5, mu, sigma) - pnorm(0, mu, sigma)

pbinom(4, size = n, prob = p)

# Section 5.8
# sampling candies

weights <- c(2, 5, 8, 14, 18)
proportion <- c(.15, .35, .2, .15, .15)
sample(weights, size = 10, prob = proportion, replace = TRUE)

