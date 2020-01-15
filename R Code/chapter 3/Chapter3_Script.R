# chapter 3 code

# Section 3.1
# the three card problem

set.seed(123)
df <- data.frame(Card = c("Blue", "Blue",
                         "Pink", "Pink",
                         "Mixed", "Mixed"),
                Side = c("Blue", "Blue",
                         "Pink", "Pink",
                         "Blue", "Pink"))
cards <- df[sample(6, size = 1000, replace = TRUE), ]
table(cards$Card, cards$Side)

# rolling two dice

df <- data.frame(Roll_1 = sample(6, size = 1000,
                                 replace = TRUE),
                 Roll_2 = sample(6, size = 1000,
                                 replace = TRUE))
df %>%
  mutate(Sum = Roll_1 + Roll_2) %>%
  filter(Roll_1 > 3) %>%
  group_by(Sum) %>%
  summarize(Count = n()) %>%
  mutate(Probability = Count / sum(Count))

# Section 3.3
# two spinners

df <- data.frame(Spin_1 = sample(4, size = 1000,
                                 replace = TRUE),
                 Spin_2 = sample(4, size = 1000,
                                 replace = TRUE))

df %>%
  mutate(Min = pmin(Spin_1, Spin_2),
         Max = pmax(Spin_1, Spin_2)) %>%
  group_by(Min, Max) %>%
  summarize(n = n()) %>%
  spread(Max, n)

# Section 3.4
# choosing balls from a random bowl

Bowl <- sample(1:2, size = 1000, replace = TRUE,
               prob = c(1, 3) / 4)
Color_1 <- sample(c("white", "black"), size = 1000,
                  replace = TRUE,
                  prob = c(1, 5) / 6)
Color_2 <- sample(c("white", "black"), size = 1000,
                  replace = TRUE,
                  prob = c(4, 2) / 6)
Color <- ifelse(Bowl == 1, Color_1, Color_2)
table(Bowl, Color)

# Section 3.7
# Bayes' rule - learning about a spinner

library(ProbBayes)
areas <- c(2, 1, 2, 1, 2)
spinner_plot(areas)

(p_dist <- spinner_probs(areas))

s_reg_A <- c(2, 2, 2, 2)
s_reg_B <- c(4, 1, 1, 2)
s_reg_C <- c(2, 4, 2)
s_reg_D <- c(1, 3, 3, 1)

(bayes_table <- data.frame(Model=c("Spinner A", "Spinner B",
                                   "Spinner C", "Spinner D")))

bayes_table$Prior <- rep(1/4, 4)
bayes_table

prob_plot(bayes_table)

# observe a spin in Region 1

bayes_table$Likelihood <- c(1/4, 1/2, 1/4, 1/8)

(bayesian_crank(bayes_table) -> bayes_table)

prior_post_plot(bayes_table)









