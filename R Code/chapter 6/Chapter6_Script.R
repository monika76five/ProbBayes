
# Section  6.2
# sampling from a box

box <- c("white", "white", "white", "white",
         "red", "red", "red", 
         "black", "black", "black")
one_rep <- function(){
  balls <- sample(box, size = 5, replace = FALSE)
  X <- sum(balls == "red")
  Y <- sum(balls == "white")
  c(X, Y)
}

set.seed(123)
results <- data.frame(t(replicate(1000, one_rep())))
names(results) <- c("X", "Y")
table(results$X, results$Y) / 1000

# conditional distribution

results %>% 
  filter(X == 2) %>% 
  group_by(Y) %>% 
  summarize(N = n()) %>% 
  mutate(P = N / sum(N))

# Section 6.3

# computing a multinomial probability

factorial(10) / (factorial(3) * factorial(5) * factorial(2)) *
    (1 / 6) ^ 3 * (2 / 6) ^ 5 * (3 / 6) ^ 2

# simulating multinomial experiments

sim_die_rolls <- function(){
  rolls <- sample(1:3, size = 10,
                  replace = TRUE,
                  prob = c(1, 2, 3) / 6)
  c(sum(rolls == 1),
    sum(rolls == 2),
    sum(rolls == 3))
}

results <- data.frame(t(replicate(5000,
                                  sim_die_rolls())))
names(results) <- c("X1", "X2", "X3")
head(results)

# probability P(X1 + X2 < 5)

results %>%
  summarize(P = sum(X1 + X2 < 5) / 5000)

# conditional distribution [X1 | X2 = 3]

results %>%
  filter(X2 == 3) %>%
  summarize(X1_M = mean(X1))

# Section 6.6

# simulating from the beta-binomial distribution

data.frame(p = rbeta(500, 6, 6)) %>% 
  mutate(Y = rbinom(500, size = 20, prob = p)) %>% 
  ggplot(aes(p, Y)) + geom_jitter()

# computing a probability

choose(20, 10) * beta(10 + 6, 26 - 10) /   beta(6, 6)

# Section 6.7

# simulating a bivariate normal

sim_binorm <- function(mx, my, sx, sy, r){
  require(ProbBayes)
  v <- matrix(c(sx ^ 2, r * sx * sy, 
                r * sx * sy, sy ^ 2),
              2, 2)
  as.data.frame(rmnorm(1000, mean = c(mx, my), 
                       varcov = v))}
mx <- 17; my <- 23; sx <- 2; sy <- 3; r <- 0.4
sdata <- sim_binorm(mx, my, sx, sy, r)
names(sdata) <- c("X", "Y")
sdata %>% summarize(mean(Y > 1.5 * X))
mean(Y > 1.5 * X)



