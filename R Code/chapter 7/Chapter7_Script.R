# Chapter 7 code

# Load some packages

library(gridExtra)
library(ProbBayes)
library(tidyverse)
crcblue <- "#2905a1"


## 7.1 Introduction:  Thinking Subjectively about a Proportion

Prior1 <- data.frame(p = seq(0.1, 1, length.out = 10),
                     Probability = rep(0.1, 10),
                     Type = "Prior 1")
Prior2 <- data.frame(p = seq(0.1, 1, length.out = 10),
                     Probability = c(0.05, 0.05, 0.05,
    0.175, 0.175, 0.175, 0.175, 0.05, 0.05, 0.05),
                     Type = "Prior 2")
bothpriors <- rbind(Prior1, Prior2)

ggplot(bothpriors, aes(p, Probability, color = Type)) +
  geom_segment(aes(xend = p, yend = 0), size = 8,
               lineend = "butt",
               color = crcblue) +
  facet_wrap(~Type, ncol = 1) +
  theme(text=element_text(size=18))


## 7.2 Bayesian Inference with Discrete Priors

bayes_table <- data.frame(p = seq(.3, .8, by=.1),
                          Prior = c(1, 1, 2, 2, 1, 1))
bayes_table


## --------------------------------------------------------
bayes_table %>% mutate(Prior = Prior / sum(Prior)) -> bayes_table
bayes_table


## --------------------------------------------------------
ggplot(data=bayes_table, aes(x=p, y=Prior)) +
  geom_bar(stat="identity", fill=crcblue, width = 0.06)


## --------------------------------------------------------
bayes_table$Likelihood <- dbinom(12, size=20, prob=bayes_table$p)
bayes_table


## --------------------------------------------------------
bayesian_crank(bayes_table) -> bayes_table
bayes_table


## --------------------------------------------------------
prior_post_plot(bayes_table, Color = crcblue) +
  theme(text=element_text(size=18))


## --------------------------------------------------------
sum(bayes_table$Posterior[bayes_table$p > 0.5])


## 7.3 Continuous Priors

betapars <- matrix(c(0.5, 0.5,
                     0.5, 1,
                     0.5, 2,
                     1, 0.5,
                     1, 1,
                     1, 2,
                     4, 0.5,
                     4, 1,
                     4, 2),
                     9, 2, byrow = TRUE)
p <- seq(.001, .999, length.out = 100)
BETA <- NULL
for (j in 1:9){
  df <- data.frame(p = p, Density = dbeta(p,
                    betapars[j, 1], betapars[j, 2]))
  df$Type <-  paste("Beta(", betapars[j, 1],
                    ",", betapars[j, 2], ")",
                    sep = "")
  BETA <- rbind(BETA, df)
}
ggplot(BETA, aes(p, Density)) +
  geom_line(color = crcblue, size = 1.5) +
  facet_wrap(~ Type, scale = "free") +
  increasefont() +
  theme(axis.text.y=element_blank()) +
  scale_x_continuous(breaks=seq(0, 1, 0.5))


## --------------------------------------------------------
dbeta(c(0.5, 0.8, 1.2), 1, 1)


## --------------------------------------------------------
pbeta(c(0.5, 0.8), 1, 1)
pbeta(0.8, 1, 1) - pbeta(0.5, 1, 1)


## --------------------------------------------------------
qbeta(c(0.5, 0.8), 1, 1)


## --------------------------------------------------------
set.seed(123)
rbeta(5, 1, 1)


## --------------------------------------------------------
beta_area(0.4, 0.8, c(7, 10), Color = crcblue)
pbeta(0.8, 7, 10) - pbeta(0.4, 7, 10)


## --------------------------------------------------------
beta_quantile(0.5, c(7, 10), Color = crcblue)
qbeta(0.5, 7, 10)


## --------------------------------------------------------
set.seed(123)
Beta44samples <- rbeta(1000, 4, 4)
Beta29samples <- rbeta(1000, 2, 9)
df1 <- data.frame(P = Beta44samples, Type = "Beta(4, 4)")
df2 <- data.frame(P = Beta29samples, Type = "Beta(2, 9)")
df <- rbind(df1, df2)
ggplot(df, aes(P)) +
  geom_histogram(fill = crcblue, color = "white",
                 bins = 15) +
  facet_wrap(~ Type, ncol = 1) + theme(text=element_text(size=18))


## --------------------------------------------------------
quantile(x = Beta44samples, c(0.25, 0.75))


## --------------------------------------------------------
beta.select(list(x = 0.55, p = 0.5),
            list(x = 0.80, p = 0.9))


## --------------------------------------------------------
beta_interval(0.5, c(3.06, 2.56), Color = crcblue)



## 7.4 Updating the Beta Prior

ab <- c(3.06, 2.56)
yny <- c(12, 8)
ab_new <- ab + yny

beta_prior_post(ab, ab_new) +
 theme(text=element_text(size=18))


## 7.5 Bayesian Inferences with Continuous Priors

ab[1]/sum(ab)
ab_new[1]/sum(ab_new)


## --------------------------------------------------------
beta_area(lo = 0.75, hi = 1.0,
          shape_par = c(15.06, 10.56),
          Color = crcblue)


## --------------------------------------------------------
set.seed(123)
S = 1000
BetaSamples = rbeta(S, 15.06, 10.56)

sum(BetaSamples >= 0.75)/S


## --------------------------------------------------------
set.seed(1234)
a <- 15.06
b <- 10.56
df <- rbind(data.frame(S = "S = 100",
                       p = rbeta(100, a, b)),
            data.frame(S = "S = 500",
                       p = rbeta(500, a, b)),
            data.frame(S = "S = 1000",
                       p = rbeta(1000, a, b)),
            data.frame(S = "S = 10000",
                       p = rbeta(10000, a, b)))

ggplot(data = data.frame(x = 0),
       mapping = aes(x = x)) +
  geom_histogram(data = df,
                 bins = 20,
                 aes(x = p, y = ..density..),
                 color="black", fill="white") +
  xlim(0, 1) +
  stat_function(fun = dbeta,
                args = list(shape1 = a, shape2 = b),
                color=crcblue) +
  facet_wrap(~ S, ncol=2) + xlab("p") +
  ylab("Density") + theme(text=element_text(size=18))


## --------------------------------------------------------
beta_interval(0.9, c(15.06, 10.56), Color = crcblue)


## --------------------------------------------------------
qbeta(c(0.05, 0.95), 15.06, 10.56)


## --------------------------------------------------------
qbeta(c(0.00, 0.90), 15.06, 10.56)


## --------------------------------------------------------
set.seed(123)
S = 1000
BetaSamples = rbeta(S, 15.06, 10.56)
quantile(BetaSamples, c(0.05, 0.95))


## --------------------------------------------------------
a <- 15.06
b <- 10.56
y <- 12; n <- 20
prob <- pbetap(c(a + y, b + n - y), 20, 0:20)

prob_plot(data.frame(Y = 0:20, Probability = prob),
          Color = crcblue, Size = 4) +
  theme(text=element_text(size=18))


## --------------------------------------------------------
discint(cbind(0:20, prob), .9)

data.frame(Y = 0:20, Probability = round(prob, 3))


## --------------------------------------------------------
set.seed(123)
a <- 3.06; b <- 2.56
n <- 20; y <- 12
pred_p_sim <- rbeta(1, a + y, b + n - y)
(pred_y_sim <- rbinom(1, n, pred_p_sim))


## --------------------------------------------------------
set.seed(123)
a <- 3.06; b <- 2.56
n <- 20; y <- 12
S = 1000
pred_p_sim <- rbeta(S, a + y, b + n - y)
pred_y_sim <- rbinom(S, n, pred_p_sim)


## --------------------------------------------------------
data.frame(Y = pred_y_sim) %>%
  group_by(Y) %>% summarize(N = n()) %>%
  mutate(Probability = N / sum(N),
         Type = "Simulated")  %>%
  select(Type, Y, Probability) -> S1

S2 <- data.frame(Type = "Exact",
                 Y = 0:20,
                 Probability = prob)

S <- rbind(S1, S2)
ggplot(S, aes(Y, Probability)) +
  geom_segment(aes(xend = Y, yend = 0),
               size = 3,
               lineend = "butt",
               color = crcblue) +
  facet_wrap(~ Type, ncol=1) +
  theme(text=element_text(size=18))


## --------------------------------------------------------
discint(as.matrix(S1[, 2:3]), .9)


## 7.6 Predictive Checking

df <- data.frame(y = 0:20,
        Probability = pbetap(c(3.06, 2.56), 20, 0:20))

prob_plot(df, Color = crcblue, Size = 3) +
  geom_point(data = data.frame(y = 12, Probability = 0),
             size = 7) +
  theme(text=element_text(size=18)) +
  annotate(geom = "text", x = 12, y = -0.01,
           label = "OBS", size = 6)


## --------------------------------------------------------
df <- data.frame(y = 0:20,
        Probability = pbetap(c(2.07, 7.32), 20, 0:20))

prob_plot(df, Color = crcblue, Size = 3) +
  geom_point(data = data.frame(y = 12, Probability = 0),
             size = 7) +
  theme(text=element_text(size=18)) +
  annotate(geom = "text", x = 12, y = -0.01,
           label = "OBS", size = 6) +
  centertitle("red")


## --------------------------------------------------------
probs <- c(0.5, 0.5)
beta_par1 <- c(3.06, 2.56)
beta_par2 <- c(2.07, 7.32)
beta_par <- rbind(beta_par1, beta_par2)
output <- binomial.beta.mix(probs, beta_par, c(12, 8))
(posterior_odds <- output$probs[1] / output$probs[2])

