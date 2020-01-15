library(tidyverse)
library(ProbBayes)

# Section 4.2
# Peter-Paul game

one_play <- function(){
  flips <- sample(c("H", "T"), 
                  size = 5,
                  replace = TRUE)
  2 * sum(flips == "H") -
       2 * sum(flips == "T")
}
G <- replicate(1000, one_play())

# Section 4.3
# expected winning

G[1:100]
mean(G)

# Section 4.4
# simulating rolls of fair and loaded dice

die1 <- c(1, 1, 1, 1, 1, 1) / 6
die2 <- c(1, 1, 4, 4, 1, 1) / 12
rolls1 <- sample(1:6, prob = die1,
                 size = 50, 
                 replace = TRUE)
rolls2 <- sample(1:6, prob = die2,
                 size = 50, 
                 replace = TRUE)

c(mean(rolls1), sd(rolls1))
c(mean(rolls2), sd(rolls2))

# simulating rolls of ten dice

roll10 <- function(){
  sum(sample(1:6, size = 10, replace = TRUE))
}
sum_rolls <- replicate(1000, roll10())

sum(sum_rolls > 29.6 & sum_rolls < 40.4) / 1000
sum(sum_rolls > 24.2 & sum_rolls < 45.8) / 1000

# Section 4.5
# binomial calculations

library(tidyverse)
data.frame(x = 0:5) %>% 
  mutate(Probability = dbinom(x, size = 5, prob = .3))

pbinom(2, size = 5, prob = .3)

1 - pbinom(1, size = 5, prob = .3)

(hits <- rbinom(50, size = 5, prob = 0.3))
table(hits)

hits <- rbinom(50, size = 5, prob = 0.3)
mean(hits)
sd(hits)

# negative binomial calculations

dnbinom(3, size = 2, prob = .372)
rnbinom(10, size = 2, prob = .372)

