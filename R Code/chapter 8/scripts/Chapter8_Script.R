# Chapter 8 Code

library(tidyverse)
library(ProbBayes)
crcblue <- "#2905a1"



## Section 8.2 Modeling Measurements

normal_draw(c(0, 1), Color = crcblue) +
  xlab(expression(mu)) +
  theme(axis.title = element_text(size = 18),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  ggtitle("")


## Section 8.3 Bayesian Inference with Discrete Priors

mu <- 15:22
normal_par <- vector(mode = "list", length = 8)
for(j in 1:8){
  normal_par[[j]] <- c(mu[j], 4)
}
many_normal_plots(normal_par) +
  xlab(expression(mu))  +
  ylab("") + increasefont() +
  theme(text=element_text(size=18))


## --------------------------------------------------------
df <- data.frame(mu = seq(15, 22, 1),
                 Prior = rep(1/8, 8)) %>%
  mutate(Likelihood = dnorm(mu, 17.2, 4 / sqrt(20)))

df <- bayesian_crank(df)
round(df, 4)


## --------------------------------------------------------
prior_post_plot(df, Color = crcblue) +
                  theme(text=element_text(size=18))



## Section 8.4 Continuous Priors

par1 <- c(18, 0.4)
par2 <- c(18, 2)
many_normal_plots(list(par1, par2)) +
  increasefont() +
  annotate(geom = "text", x = 21, y = 0.75,
           label = "N(18, 0.4)", size = 6) +
  annotate(geom = "text", x = 21, y = 0.2,
           label = "N(18, 2)", size = 6)


## --------------------------------------------------------
normal.select(list(p = 0.5, x = 18), list(p = 0.9, x = 20))


## --------------------------------------------------------
normal_draw(c(18, 1.56), Color = crcblue) +
  xlab(expression(mu)) +
  ggtitle("") +
  increasefont()


## Section 8.5 Updating the Normal Prior

prior <- c(18, 1.56)
data <- c(17.20, 0.89)
normal_update(prior, data)


## --------------------------------------------------------
par1 <- c(18, 1.56)
par2 <- c(17.4, 0.77)
ggplot(data.frame(x = c(10, 25)), aes(x)) +
  stat_function(fun = dnorm, size = 1.5,
                linetype = "dashed",
                args = list(mean = 18, sd = 1.56)) +
  stat_function(fun = dnorm, size = 1.5,
                args = list(mean = 17.4, sd = 0.77)) +
  increasefont() +
  xlab(expression(mu))  +
  annotate(geom = "text", x = 21, y = 0.2,
           label = "Prior", size = 6) +
  annotate(geom = "text", x = 20, y = 0.45,
           label = "Posterior", size = 6)


## Section 8.6 Bayesian Inferences for Continuous Normal Mean

1 - pnorm(19, 17.4, 0.77)


## --------------------------------------------------------
set.seed(123)
S <- 1000
NormalSamples <- rnorm(S, 17.4, 0.77)
sum(NormalSamples >= 19) / S


## --------------------------------------------------------
qnorm(c(0.05, 0.95), 17.4, 0.77)


## --------------------------------------------------------
set.seed(123)
S <- 1000
NormalSamples <- rnorm(S, 17.4, 0.77)
quantile(NormalSamples, c(0.05, 0.95))


## --------------------------------------------------------
qnorm(c(0.025, 0.975), 17.4, 0.77)


## --------------------------------------------------------
set.seed(123)
sigma <- 4
mu_n <- 17.4
sigma_n <- 0.77
pred_mu_sim <- rnorm(1, mu_n, sigma_n)
(pred_y_sim <- rnorm(1, pred_mu_sim, sigma))


## --------------------------------------------------------
set.seed(123)
S = 1000
pred_mu_sim <- rnorm(S, mu_n, sigma_n)
pred_y_sim <- rnorm(S, pred_mu_sim, sigma)
pred_mean <- mu_n
pred_sd <- sqrt(sigma ^ 2 + sigma_n ^ 2)
x_grid <- seq(pred_mean - 3 * pred_sd,
              pred_mean + 3 * pred_sd, length.out = 100)
dest <- density(pred_y_sim)

df1 <- data.frame(Prediction = dest$x,
                  Density = dest$y,
                  Type = "Simulated")
df2 <- data.frame(Prediction = dest$x,
                  Density = dnorm(dest$x,
                                  pred_mean, pred_sd),
                  Type = "Exact")

ggplot(rbind(df1, df2),
       aes(Prediction, Density, linetype = Type)) +
  geom_line(size = 1.5) +
  theme(text=element_text(size=18))



## Section 8.7 Posteriori Predictive Checking

set.seed(123)
sigma <- 4
mu_n <- 17.4
sigma_n <- 0.77
S <- 1000
pred_mu_sim <- rnorm(S, mu_n, sigma_n)
sim_ytilde <- function(j){
  rnorm(20, pred_mu_sim[j], sigma)
}
ytilde <- t(sapply(1:S, sim_ytilde))


## --------------------------------------------------------
pred_ybar_sim <- apply(ytilde, 1, mean)
ybar <- 17.20


## --------------------------------------------------------
library(latex2exp)
ggplot(data.frame(Ybar = pred_ybar_sim), aes(Ybar)) +
  geom_density(size = 1.5) +
  geom_vline(xintercept = ybar, size = 1.5) +
  annotate(geom = "text", x = 16.5, y = .32,
           label = TeX("$\\bar{Y}_{OBS}$"),
           size = 6) +
  theme(text=element_text(size=18)) +
  ylab("Density") +
  xlab(TeX("$\\bar{Y}$"))


## Section 8.8 Modeling Count Data

ggplot(data.frame(x = c(30, 150)), aes(x)) +
  stat_function(fun = dgamma, color = crcblue,
                size = 1.5,
                args = list(shape = 80, rate =1)) +
  stat_function(fun = dgamma, color = crcblue,
                size = 1.5,
                args = list(shape = 40, rate = 0.5)) +
  stat_function(fun = dgamma, color = crcblue,
                size = 1.5,
                args = list(shape = 20, rate = 0.25)) +
  theme(text=element_text(size=18)) +
  annotate(geom = "text", x = 103, y = 0.04,
           label = TeX("$(\\alpha,\\,  \\beta$) = (80, 1)"), size = 7 ) +
  annotate(geom = "text", x = 113, y = 0.025,
           label = TeX("$(\\alpha,\\,  \\beta$) = (40, 0.5)"), size = 7 ) +
  annotate(geom = "text", x = 127, y = 0.01,
           label = TeX("$(\\alpha,\\,  \\beta$) = (20, 0.25)"), size = 7 ) +
  xlab(TeX("$\\lambda"))  +
  ylab("Density")


## --------------------------------------------------------
web_visits <- read_csv("../data/web_visits.csv")


## --------------------------------------------------------
y <- filter(web_visits, Day %in% c("Mon", "Tue",
                    "Wed", "Thu", "Fri")) %>%
    select(Count)  %>%  pull()


## --------------------------------------------------------
alpha <- 80
beta <- 1
alpha1 <- alpha + sum(y)
beta1 <- beta + length(y)


## --------------------------------------------------------
X <- qgamma(c(.05, 0.95), shape = alpha1, rate = beta1)
x0 <- seq(X[1], X[2], length.out = 100)
xx <- c(x0, X[2], X[1])
yy <- c(dgamma(x0, shape = alpha1, rate = beta1), 0, 0)
ggplot(data.frame(x = c(90, 120)), aes(x)) +
  stat_function(fun = dgamma,
                args = list(shape = 2200, rate = 21)) +
  increasefont() +
  xlab(expression(lambda)) +
  ylab("Density") +
  geom_polygon(data=data.frame(xx, yy), aes(xx, yy),
               fill=crcblue)


## --------------------------------------------------------
lambda <- rgamma(1000, shape = alpha1, rate = beta1)
ys <- rpois(1000, lambda)
quantile(ys, c(.05, .95))


## --------------------------------------------------------
library(latex2exp)
ggplot(data.frame(Y=ys), aes(Y)) +
  geom_bar(width = 0.6, fill = crcblue) +
  increasefont() +
  xlab(TeX("$\\tilde{Y}$"))


## Exercise 18

ggplot(data.frame(x = c(2, 20)), aes(x)) +
  stat_function(fun = dgamma, linetype = 1, size = 1.5,
                args = list(shape = 100/3, rate = 10/3)) +
  stat_function(fun = dgamma, linetype = 2, size = 1.5,
                args = list(shape = 70, rate = 10)) +
  xlab(expression(lambda)) + ylab("Density") +
  annotate(geom = "text", x = 12, y = .4, size = 6,
           label = "Pedro's Prior: G(70, 10)") +
  annotate(geom = "text", x = 15, y = .2, size = 6,
           label = "Mia's Prior: G(33.3, 3.3)") +
  theme(axis.title.x = element_text(size = 18)) +
  increasefont()

