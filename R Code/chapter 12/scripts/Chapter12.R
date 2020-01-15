## ----setup, include=FALSE--------------------------------

library(tidyverse)
require(gridExtra)
require(runjags)
require(coda)
library(ggridges)
library(ProbBayes)
library(vcd)
crcblue <- "#2905a1"

# Section 12.2 Bayesian Multiple Linear Regression

## ----warning=FALSE---------------------------------------
CEsample <- read_csv("../data/CEsample.csv")


## ----warning=FALSE---------------------------------------
J <- nrow(CEsample)
CEsample %>%
  mutate(log_TotalIncome = log(TotalIncomeLastYear),
         log_TotalExp = log(TotalExpLastQ),
         Rural = ifelse(UrbanRural == 2, 1, 0))  %>%
  filter() ->
  CEsample


## ----warning=FALSE---------------------------------------
ggplot(CEsample, aes(log_TotalIncome, log_TotalExp)) +
  geom_point(color = crcblue) +
  facet_wrap(~ Rural, labeller = label_both) +
  theme(text=element_text(size=18))


## ----warning=FALSE---------------------------------------
## write the model
modelString <-"
model {
## sampling
for (i in 1:N){
y[i] ~ dnorm(beta0 + beta1*x_income[i] +
              beta2*x_rural[i], invsigma2)
}
## priors
beta0 ~ dnorm(mu0, g0)
beta1 ~ dnorm(mu1, g1)
beta2 ~ dnorm(mu2, g2)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}
"


## ----warning=FALSE---------------------------------------
y <- as.vector(CEsample$log_TotalExp)
x_income <- as.vector(CEsample$log_TotalIncome)
x_rural <- as.vector(CEsample$Rural)
N <- length(y)

the_data <- list("y" = y, "x_income" = x_income,
                 "x_rural" = x_rural, "N" = N,
                 "mu0" = 0, "g0" = 0.0025,
                 "mu1" = 0, "g1" = 0.0025,
                 "mu2" = 0, "g2" = 0.0025,
                 "a" = 0.001, "b" = 0.001)


## ----warning=FALSE---------------------------------------
initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## ----warning=FALSE---------------------------------------
posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta0", "beta1",
                                  "beta2", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 20000,
                      inits = initsfunction)


## ----warning=FALSE---------------------------------------
plot(posterior, vars = "beta1")


## ----warning=FALSE---------------------------------------
print(posterior, digits = 3)


## ----warning=FALSE---------------------------------------
post <- as.mcmc(posterior)
post <- as.data.frame(post)


## ----warning=FALSE---------------------------------------
# plot some sample lines from posterior

rdata1 <- data.frame(a = post$beta0[1:10],
                     b = post$beta1[1:10],
                     Rural = 0)
rdata2 <- data.frame(a = post$beta0[1:10] +
                       post$beta2[1:10],
                     b = post$beta1[1:10],
                     Rural = 1)
rdata <- rbind(rdata1, rdata2)
ggplot(CEsample, aes(log_TotalIncome, log_TotalExp)) +
  geom_point(alpha = 0.2, color = crcblue) +
  facet_wrap(~ Rural, labeller = label_both) +
  geom_abline(data=rdata,
              aes(intercept=a, slope=b),
              alpha = 0.5, color = crcblue) +
  xlab("log Total Income") +
  ylab("log Total Expenditure") +
  theme_grey(base_size = 18, base_family = "")


## ----warning=FALSE---------------------------------------
post <- as.mcmc(posterior)
post <- as.data.frame(post)
one_expected <- function(x1, x2){
  lp <- post[ , "beta0"] +  x1 * post[ , "beta1"] +
     x2 * post[, "beta2"]
  data.frame(Value = paste("Log Income =", x1,
                           "Rural =", x2),
             Expected_log_TotalExp = lp)
}

df <- map2_df(c(9, 12, 9, 12),
              c(0, 0, 1, 1), one_expected)

ggplot(df, aes(x = Expected_log_TotalExp, y = Value)) +
  geom_density_ridges() +
  theme(text=element_text(size=18))


## ----warning=FALSE---------------------------------------
one_predicted <- function(x1, x2){
  lp <- post[ , "beta0"] +  x1 * post[ , "beta1"] +
    x2 * post[, "beta2"]
  y <- rnorm(5000, lp, post[, "sigma"])
  data.frame(Value = paste("Log Income =", x1,
                           "Rural =", x2),
             Predicted_log_TotalExp = y)
}
df <- map2_df(c(12, 12),
              c(0, 1), one_predicted)

ggplot(df, aes(x = Predicted_log_TotalExp, y = Value)) +
  geom_density_ridges(color = crcblue) +
  theme_grey(base_size = 18, base_family = "")


## ----warning=FALSE---------------------------------------
df %>% group_by(Value) %>%
  summarize(P05 = quantile(Predicted_log_TotalExp, .05),
            P95 = quantile(Predicted_log_TotalExp, .95))


## ----warning=FALSE---------------------------------------
one_expected <- function(x1, x2){
  lp <- post[ , "beta0"] +  x1 * post[ , "beta1"] +
    x2 * post[, "beta2"]
  data.frame(Value = paste("Log Income =", x1,
                           "Rural =", x2),
             Expected_log_TotalExp = lp)
}

df2 <- map2_df(c(12, 12),
              c(0, 1), one_expected)

df2 %>% group_by(Value) %>%
  summarize(P05 = quantile(Expected_log_TotalExp, .05),
            P95 = quantile(Expected_log_TotalExp, .95))

# Section 12.3 Comparing Regression Models

## --------------------------------------------------------
modelString = "
model {
for (i in 1:N){
   y[i] ~ dnorm(mu[i], phi)
   mu[i] <- beta0 + beta1 * (x[i] - 30) +
            beta2 * pow(x[i] - 30, 2)
}
beta0 ~ dnorm(0, 0.001)
beta1 ~ dnorm(0, 0.001)
beta2 ~ dnorm(0, 0.001)
phi ~ dgamma(0.001, 0.001)
}
"


## --------------------------------------------------------
d <- filter(sluggerdata,
            Player == "Schmidt", AB >= 200)
the_data <- list(y = d$HR / d$AB,
                 x = d$Age,
                 N = 16)


## --------------------------------------------------------
initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## --------------------------------------------------------
post2 <- run.jags(modelString,
                      n.chains = 2,
                      data = the_data,
                      monitor = c("beta0", "beta1",
                                  "beta2", "phi"),
                      inits = initsfunction)


## --------------------------------------------------------
D2 <- extract.runjags(post2, "dic") # -84.2
D2

# Section 12.4 Bayesian Logistic Regression

ab1 <- beta.select(list(p = 0.5, x = 0.1),
            list(p = 0.9, x = 0.2))

ab2 <- beta.select(list(p = 0.5, x = 0.7),
                   list(p = 0.9, x = 0.8))

df <- data.frame(x1 = c(20, 80),
                 y1 = c(qbeta(.05, ab1[1], ab1[2]),
                        qbeta(.05, ab2[1], ab2[2])),
                 x2 = c(20, 80),
                 y2 = c(qbeta(.95, ab1[1], ab1[2]),
                        qbeta(.95, ab2[1], ab2[2])))
df0 <- data.frame(x = c(20, 80),
                  y = c(qbeta(.5, ab1[1], ab1[2]),
                        qbeta(.5, ab2[1], ab2[2])))

ggplot(df) +
  geom_segment(aes(x = x1, y = y1,
                   xend = x2, yend = y2),
               size = 2) +
  geom_point(data = df0, aes(x, y),
             size = 5) +
  xlim(0, 100) + ylim(0, 1) +
  increasefont() +
  xlab("Family Income (Unit = $1000)") +
  ylab("Prob(Participation)")

modelString <-"
model {
## priors
beta1 <- (logit(p1) - logit(p2)) / (x1 - x2)
beta0 <- logit(p1) - beta1 * x1
p1 ~ dbeta(a1, b1)
p2 ~ dbeta(a2, b2)
}
"

the_data <- list("a1" = ab1[1], "b1" = ab1[2],
                 "a2" = ab2[1], "b2" = ab2[2],
                 "x1" = 20, "x2" = 80)

library(runjags)

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}

prior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta0", "beta1"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      inits = initsfunction)

library(coda)
prior <- as.mcmc(prior)
plot(prior)

library(ggplot2)
ggplot(as.data.frame(prior), aes(beta0, beta1)) +
  geom_point() +
  increasefont()


## posterior analysis
LaborParticipation <- read_csv("../data/LaborParticipation.csv")

## write the model

modelString <-"
model {
## sampling
for (i in 1:N){
y[i] ~ dbern(p[i])
logit(p[i]) <- beta0 + beta1*x[i]
}
## priors
beta1 <- (logit(p1) - logit(p2)) / (x1 - x2)
beta0 <- logit(p1) - beta1 * x1
p1 ~ dbeta(a1, b1)
p2 ~ dbeta(a2, b2)
}
"

y <- as.vector(LaborParticipation$Participation)
x <- as.vector(LaborParticipation$FamilyIncome)
N <- length(y)
the_data <- list("y" = y, "x" = x, "N" = N,
                 "a1" = ab1[1], "b1" = ab1[2],
                 "a2" = ab2[1], "b2" = ab2[2],
                 "x1" = 20, "x2" = 80)


ggplot(data.frame(x, y), aes(x, y)) +
  geom_jitter(height = 0.05) +
  xlab("Family Income (Unit = $1000)") +
  ylab("Participation") +
  increasefont()


library(runjags)

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}

posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta0", "beta1"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      inits = initsfunction)

options(digits = 4)
summary(posterior)

plot(posterior, vars = "beta0")
plot(posterior, vars = "beta1")


  post <- as.matrix(as.mcmc(posterior))

  prob_interval <- function(x, post){
    lp <- post[, 1] + x * post[, 2]
    quantile(exp(lp) / (1 + exp(lp)),
             c(.05, .50, .95))
  }

  out <- sapply(seq(10, 70, by = 10),
                 prob_interval, post)

  df_out <- data.frame(Income = seq(10, 70, by = 10),
                       Low = out[1, ],
                       M = out[2, ],
                       Hi = out[3,  ])


  ggplot(df_out) +
    geom_line(aes(x = Income, y = M)) +
    geom_segment(aes(x = Income, y = Low,
                     xend = Income, yend = Hi),
                 size = 2) +
    ylab("Prob(Participate)") +
    ylim(0, 1)

  # prediction

  prediction_interval <- function(x, post, n = 20){
    lp <- post[, 1] + x * post[, 2]
    p <- exp(lp) / (1 + exp(lp))
    y <- rbinom(length(p), size = n, prob = p)
    quantile(y / n,
             c(.05, .50, .95))
  }

  out <- sapply(seq(10, 70, by = 10),
                prediction_interval, post, n = 50)

  df_out <- data.frame(Income = seq(10, 70, by = 10),
                       Low = out[1, ],
                       M = out[2, ],
                       Hi = out[3,  ])

  ggplot(df_out) +
    geom_line(aes(x = Income, y = M)) +
    geom_segment(aes(x = Income, y = Low,
                     xend = Income, yend = Hi),
                 size = 2) +
    ylab("Proportion of Labor Participation") +
    ylim(0, 1)


