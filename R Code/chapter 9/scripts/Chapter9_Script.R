
library(ggplot2)
library(gridExtra)
library(ProbBayes)
library(VGAM)
library(tidyverse)
library(coda)
library(reshape2)
library(ggridges)
library(runjags)
library(bayesplot)
#library(CalledStrike)
crcblue <- "#2905a1"

# Section 9.1 Introduction

## --------------------------------------------------------
buffalo <- read_csv("../data/buffalo_snowfall.csv")
data <- buffalo[59:78, c("SEASON", "JAN")]


## --------------------------------------------------------
ybar <- mean(data$JAN)
se <- sd(data$JAN) / sqrt(20)
(post1 <- normal_update(c(10, 3), c(ybar, se)))


## --------------------------------------------------------
many_normal_plots(list(c(10, 3),
                       c(ybar, se),
                       post1)) +
  theme(legend.position = "none") +
  theme(text=element_text(size=18))  +
  xlab(expression(mu)) +
  annotate(geom = "text",
           x = 10, y = 0.15,
           label = "Prior", size = 7) +
  annotate(geom = "text",
           x = 35, y = 0.10,
           label = "Likelihood", size = 7) +
  annotate(geom = "text",
           x = 25, y = 0.15,
           label = "Posterior", size = 7) +
  ylab("Density")


## --------------------------------------------------------
ggplot(data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = dnorm, size = 1.5,
                args = list(mean = 10, sd = 3)) +
  stat_function(fun = dcauchy, size = 1.5,
                args = list(location = 10, scale = 2),
                linetype = "dashed") +
  xlab(expression(mu)) +
  annotate(geom = "text",
           x = 5.5, y = 0.10,
           label = "Normal", size = 7) +
  annotate(geom = "text",
           x = 13.5, y = 0.14,
           label = "Cauchy", size = 7) +
  theme(text=element_text(size=18))


# Section 9.2 Markov Chains

## --------------------------------------------------------
p <- c(0, 0, 1, 0, 0, 0)

P <- matrix(c(.5, .5, 0, 0, 0, 0,
              .25, .5, .25, 0, 0, 0,
              0, .25, .5, .25, 0, 0,
              0, 0, .25, .5, .25, 0,
              0, 0, 0, .25, .5, .25,
              0, 0, 0, 0, .5, .5),
            nrow=6, ncol=6, byrow=TRUE)


## --------------------------------------------------------
print(p %*% P, digits = 5)


## --------------------------------------------------------
print(p %*% P %*% P %*% P %*% P, digits = 5)


## --------------------------------------------------------
Pm <- diag(rep(1, 6))
for(j in 1:100){
  Pm <- Pm %*% P
}
print(Pm, digits = 5)


## --------------------------------------------------------
set.seed(123)
s <- vector("numeric", 10000)
s[1] <- 3
for (j in 2:10000){
  s[j] <- sample(1:6, size=1, prob=P[s[j - 1], ])
}
S <- data.frame(Iteration = 1:10000,
                Location = s)


## --------------------------------------------------------
S %>% mutate(L1 = (Location == 1),
             L2 = (Location == 2),
             L3 = (Location == 3),
             L4 = (Location == 4),
             L5 = (Location == 5),
             L6 = (Location == 6))  %>%
  mutate(Proportion_1 = cumsum(L1) / Iteration,
         Proportion_2 = cumsum(L2) / Iteration,
         Proportion_3 = cumsum(L3) / Iteration,
         Proportion_4 = cumsum(L4) / Iteration,
         Proportion_5 = cumsum(L5) / Iteration,
         Proportion_6 = cumsum(L6) / Iteration) %>%
  select(Iteration, Proportion_1, Proportion_2, Proportion_3,
         Proportion_4, Proportion_5, Proportion_6) -> S1

gather(S1, Outcome, Probability, -Iteration) -> S2

ggplot(S2, aes(Iteration, Probability)) +
  geom_line() +
  facet_wrap(~ Outcome, ncol = 3) +
  ylim(0, .4) +
  ylab("Relative Frequency") +
  theme(text=element_text(size=18))  +
  scale_x_continuous(breaks = c(0, 3000, 6000, 9000))


## --------------------------------------------------------
w <- matrix(c(.1,.2,.2,.2,.2,.1), nrow=1, ncol=6)
w %*% P

# Section 9.3 The Metropolis Algorithm

## --------------------------------------------------------
pd <- function(x){
  values <- c(5, 10, 4, 4, 20, 20, 12, 5)
  ifelse(x %in% 1:length(values), values[x], 0)
}
prob_dist <- data.frame(x = 1:8,
                        prob = pd(1:8))


## --------------------------------------------------------
### TO UPDATE
prob_plot(prob_dist, Color = crcblue) +
  theme(text=element_text(size=18))  +
  ylab("Probability")


## --------------------------------------------------------
random_walk <- function(pd, start, num_steps){
  y <- rep(0, num_steps)
  current <- start
  for (j in 1:num_steps){
    candidate <- current + sample(c(-1, 1), 1)
    prob <- pd(candidate) / pd(current)
    if (runif(1) < prob) current <- candidate
    y[j] <- current
  }
  return(y)
}


## --------------------------------------------------------
set.seed(123)
out <- random_walk(pd, 4, 10000)
data.frame(out) %>% group_by(out) %>%
  summarize(N = n(), Prob = N / 10000) -> S


## --------------------------------------------------------
prob_dist %>% mutate(Prob = prob / sum(prob)) ->
  prob_dist
df <- rbind(data.frame(x = prob_dist$x,
                       Prob = prob_dist$Prob,
                       Type = "Actual"),
            data.frame(x = S$out, Prob = S$Prob,
                       Type = "Simulated")
)

ggplot(df, aes(x, Prob, fill=Type)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  ylab("Probability") +
  scale_fill_manual(values=c("gray", crcblue)) +
  theme(text=element_text(size=18))


## --------------------------------------------------------
## Figure 8.6


## --------------------------------------------------------
metropolis <- function(logpost, current, C, iter, ...){
  S <- rep(0, iter)
  n_accept <- 0
  for(j in 1:iter){
  candidate <- runif(1, min=current - C,
                       max=current + C)
  prob <- exp(logpost(candidate, ...) -
             logpost(current, ...))
  accept <- ifelse(runif(1) < prob, "yes", "no")
  current <- ifelse(accept == "yes",
                    candidate, current)
  S[j] <- current
  n_accept <- n_accept + (accept == "yes")
  }
  list(S=S, accept_rate=n_accept / iter)
}

# Section 9.4 Cauchy-Normal Problem

## --------------------------------------------------------
lpost <- function(theta, s){
    dcauchy(theta, s$loc, s$scale, log = TRUE) +
    dnorm(s$ybar, theta, s$se, log = TRUE)
}


## --------------------------------------------------------
buffalo <- read_csv("../data/buffalo_snowfall.csv")
data <- buffalo[59:78, c("SEASON", "JAN")]
s <- list(loc = 10, scale = 2,
          ybar = mean(data$JAN),
          se = sd(data$JAN) / sqrt(20))


## --------------------------------------------------------
out <- metropolis(lpost, 5, 20, 10000, s)
out$accept_rate


## --------------------------------------------------------
manyC <- c(0.3, 3, 30, 200)
M <- NULL
set.seed(1223)
for(j in 1:4){
  out <- metropolis(lpost, 0, manyC[j], 5000, s)
  M <- rbind(M, data.frame(X = out$S,
                           Iteration = 1:5000,
                           C = paste("C = ",manyC[j],
                   ", Accept = ", out$accept_rate)))
}


## --------------------------------------------------------
ggplot(M, aes(Iteration, X)) +
  geom_line(color = crcblue) +
  facet_wrap(~ C, nrow=2) +
  theme(text=element_text(size=18)) +
  scale_x_continuous(breaks = c(0, 2000, 4000))


## --------------------------------------------------------
mu <- seq(0, 40, length.out = 200)
prior <- dcauchy(mu, 10, 2)
likelihood <- dnorm(mu, s$ybar, s$se)
posterior <- density(out$S, bw = 0.6)
P1 <- data.frame(MU = mu,
                 Density = prior,
                 Type = "Prior")
P2 <- data.frame(MU = mu,
                 Density = likelihood,
                 Type = "Likelihood")
P3 <- data.frame(MU = posterior$x,
                Density = posterior$y,
                Type = "Posterior")
P <- rbind(P1, P2, P3)
ggplot(P, aes(MU, Density, linetype = Type)) +
  geom_line(size = 1.3, color = crcblue) +
  xlab(expression(mu)) +
  annotate(geom = "text",
           x = 4.5, y = 0.10,
           label = "Prior", size = 7) +
  annotate(geom = "text",
           x = 35, y = 0.10,
           label = "Likelihood", size = 7) +
  annotate(geom = "text",
           x = 18, y = 0.10,
           label = "Posterior", size = 7) +
  theme(text=element_text(size=18)) +
  theme(legend.position = "none")


# Section 9.5 Gibbs Sampling

## --------------------------------------------------------
p <- matrix(c(4, 3, 2, 1,
              3, 4, 3, 2,
              2, 3, 4, 3,
              1, 2, 3, 4) / 40, 4, 4, byrow = TRUE)
dimnames(p)[[1]] <- 1:4
dimnames(p)[[2]] <- 1:4
p


## --------------------------------------------------------
p[, 1]


## --------------------------------------------------------
p[2, ]


## --------------------------------------------------------
gibbs_discrete <- function(p, i = 1, iter = 1000){
  x <- matrix(0, iter, 2)
  nX <- dim(p)[1]
  nY <- dim(p)[2]
  for(k in 1:iter){
    j <- sample(1:nY, 1, prob = p[i, ])
    i <- sample(1:nX, 1, prob = p[, j])
    x[k, ] <- c(i, j)
  }
  x
}


## --------------------------------------------------------
sp <- data.frame(gibbs_discrete(p))
names(sp) <- c("X", "Y")
table(sp) / 1000


## --------------------------------------------------------
gibbs_betabin <- function(n, a, b, p = 0.5, iter = 1000){
  x <- matrix(0, iter, 2)
  for(k in 1:iter){
    y <- rbinom(1, size = n, prob = p )
    p <- rbeta(1, y + a, n - y + b )
    x[k, ] <- c(y, p)
  }
  x
}


## --------------------------------------------------------
set.seed(123)
sp <- data.frame(gibbs_betabin(20, 5, 5))
ggplot(data.frame(Y=sp$X1), aes(Y)) +
  geom_bar(width=0.5, fill=crcblue) +
  ylab("Frequency") +
  theme(text=element_text(size=18))


## --------------------------------------------------------
gibbs_normal <- function(s, phi = 0.002, iter = 1000){
  ybar <- mean(s$y)
  n <- length(s$y)
  mu0 <- s$mu0
  phi0 <- s$phi0
  a <- s$a
  b <- s$b
  x <- matrix(0, iter, 2)
  for(k in 1:iter){
    mun <- (phi0 * mu0 + n * phi * ybar) /
      (phi0 + n * phi)
    sigman <- sqrt(1 / (phi0 + n * phi))
    mu <- rnorm(1, mean = mun, sd = sigman)
    an <- n / 2 + a
    bn <- sum((s$y - mu) ^ 2) / 2 + b
    phi <- rgamma(1, shape = an, rate = bn)
    x[k, ] <- c(mu, phi)
  }
  x
}


## --------------------------------------------------------
s <- list(y = data$JAN, mu0 = 10, phi0 = 1/3^2, a = 1, b = 1)


## --------------------------------------------------------
set.seed(123)
out <- gibbs_normal(s, iter=10000)
df <- data.frame(out)
names(df) <- c("mean", "precision")
df$standard_deviation <- sqrt(1 / df$precision)


## --------------------------------------------------------
ggplot(df, aes(mean, standard_deviation)) +
  geom_point(size = 0.2, color = crcblue) +
  theme(text=element_text(size=18)) +
  xlab(expression(mu)) +
  ylab(expression(sigma))


# Section 9.6 MCMC Inputs and Diagnostics

## --------------------------------------------------------
lpost <- function(theta, s){
    dcauchy(theta, s$loc, s$scale, log = TRUE) +
    dnorm(s$ybar, theta, s$se, log = TRUE)
}
buffalo <- read_csv("../data/buffalo_snowfall.csv")
data <- buffalo[59:78, c("SEASON", "JAN")]
s <- list(loc = 10, scale = 2,
          ybar = mean(data$JAN),
          se = sd(data$JAN) / sqrt(20))


## --------------------------------------------------------
buffalo_metrop <- metropolis(lpost, 10,
                  20, 5000, s)
buffalo_metrop$accept_rate


## --------------------------------------------------------
df <- data.frame(mean = buffalo_metrop$S)
df$Iteration <- 1:5000
ggplot(df, aes(Iteration, mean)) +
  geom_line(color = crcblue) +
  increasefont() +
  ylab(expression(mu))


## --------------------------------------------------------
df_mcmc <- mcmc(df)
ac_data <- data.frame(Lag = 0:20,
                Autocorrelation =
                  autocorr(df_mcmc[, "mean"],
                           lags = 0:20)[,,1])
ggplot(ac_data, aes(Lag, Autocorrelation)) +
  geom_col(fill = crcblue, width = 0.5) +
  theme(text=element_text(size=18)) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0)


# Section 9.7 Using JAGS

## --------------------------------------------------------
modelString = "
model{
## sampling
for (i in 1:N) {
y[i] ~ dnorm(mu, phi)
}
## priors
mu ~ dnorm(mu0, phi0)
phi ~ dgamma(a, b)
sigma <- sqrt(pow(phi, -1))
}
"


## --------------------------------------------------------
buffalo <- read.csv("../data/buffalo_snowfall.csv")
data <- buffalo[59:78, c("SEASON", "JAN")]
y <- data$JAN
N <- length(y)
the_data <- list("y" = y, "N" = N,
                 "mu0"=10, "phi0"=1/3^2,
                 "a"=1,"b"=1)


## --------------------------------------------------------
initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## --------------------------------------------------------
posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("mu", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      inits = initsfunction)


## --------------------------------------------------------
plot(posterior, vars = "mu")


## --------------------------------------------------------
plot(posterior, vars = "sigma")


## --------------------------------------------------------
print(posterior, digits = 3)


## --------------------------------------------------------
InitialValues <- list(
  list(mu = 2, phi = 1 / 4),
  list(mu = 30, phi = 1 / 900)
)


## --------------------------------------------------------
posterior <- run.jags(modelString,
                      n.chains = 2,
                      data = the_data,
                      monitor = c("mu", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      inits = InitialValues)


## --------------------------------------------------------
summary(posterior$mcmc[[1]], digits = 3)


## --------------------------------------------------------
summary(posterior$mcmc[[2]], digits = 3)


## --------------------------------------------------------
post <- data.frame(posterior$mcmc[[1]])


## --------------------------------------------------------
postpred_sim <- function(j){
  rnorm(20, mean = post[j, "mu"],
        sd = post[j, "sigma"])
}


## --------------------------------------------------------
set.seed(123)
print(postpred_sim(1), digits = 3)


## --------------------------------------------------------
set.seed(123)
ypred <- t(sapply(1:5000, postpred_sim))


## --------------------------------------------------------
df <- NULL
for(j in 1:8){
  dfnew <- data.frame(Type = paste("Sample", j), Snowfall = ypred[j, ])
  df <- rbind(df, dfnew)
}
df <- rbind(df, data.frame(Type = "Observed", Snowfall = y))


## --------------------------------------------------------
ggplot(df, aes(Snowfall)) +
  geom_histogram(bins = 10, fill = crcblue, color = "white") +
  facet_wrap(~ Type, ncol = 3) +
  theme(text=element_text(size=18))


## --------------------------------------------------------
postpred_max <- apply(ypred, 1, max)


## --------------------------------------------------------
ggplot(data.frame(Maximum = postpred_max), aes(Maximum)) +
  geom_histogram(bins = 20,
                 fill = crcblue,
                 color = "white") +
  geom_vline(xintercept = max(y), size = 1.5) +
  annotate(geom = "text", x = 78, y = 750, label="Observed
  Maximum", size = 6) +
  theme(text=element_text(size=18))


## --------------------------------------------------------
modelString = "
model{
## sampling
yF ~ dbin(pF, nF)
yM ~ dbin(pM, nM)
logit(pF) <- theta - lambda / 2
logit(pM) <- theta + lambda / 2
## priors
theta ~ dnorm(mu0, phi0)
lambda ~ dnorm(0, phi)
}
"


## --------------------------------------------------------
the_data <- list("yF" = 75, "nF" = 151,
                 "yM" = 39, "nM" = 93,
                 "mu0" = 0, "phi0" = 0.001, "phi" = 2)


## --------------------------------------------------------
initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## --------------------------------------------------------
posterior <- run.jags(modelString,
                 data = the_data,
                 n.chains = 1,
                 monitor = c("pF", "pM", "lambda"),
                 adapt = 1000,
                 burnin = 5000,
                 sample = 5000)


## --------------------------------------------------------
post <- data.frame(posterior$mcmc[[1]])


## --------------------------------------------------------
ggplot(post, aes(lambda)) +
  geom_density(size = 1.5, color = crcblue) +
  geom_vline(xintercept = 0, size = 1.5) +
  theme(text=element_text(size=18)) +
  xlab(expression(lambda))


## --------------------------------------------------------
post %>%
  summarize(Prob = mean(lambda < 0))

