
library(tidyverse)
require(gridExtra)
require(CalledStrike)
require(runjags)
require(coda)
library(ggridges)
library(latex2exp)
crcblue <- "#2905a1"

# Section 11.2 Example: Prices and Areas of House Sales

## --------------------------------------------------------
PriceAreaData <- read_csv("../data/house_prices.csv")

PriceAreaData$newsize <- PriceAreaData$size / 1000

g1 <- ggplot(PriceAreaData, aes(x = newsize, y = price)) +
  geom_point(size=3) +
  labs(x = "Size (1000 sq feet)", y = "Price ($1,000)") +
  theme(text=element_text(size=18))

g1

# Section 11.6 Inference Through MCMC

## --------------------------------------------------------
modelString <-"
model {

## sampling
for (i in 1:N){
y[i] ~ dnorm(beta0 + beta1*x[i], invsigma2)
}

## priors
beta0 ~ dnorm(mu0, g0)
beta1 ~ dnorm(mu1, g1)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}
"


## --------------------------------------------------------
y <- PriceAreaData$price
x <- PriceAreaData$newsize
N <- length(y)
the_data <- list("y" = y, "x" = x, "N" = N,
                 "mu0" = 0, "g0" = 0.0001,
                 "mu1" = 0, "g1" = 0.0001,
                 "a" = 1, "b" = 1)


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
                      monitor = c("beta0",
                                  "beta1", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      inits = initsfunction)


## --------------------------------------------------------
options(digits = 4)
post <- as.mcmc(posterior)
post[1:10, ]


## --------------------------------------------------------
plot(posterior, vars = "beta0")


## --------------------------------------------------------
ggplot(as.data.frame(post), aes(beta0, beta1)) +
  geom_point() +
  theme(text=element_text(size=18)) +
  xlab(TeX('$\\beta_0')) +
  ylab(TeX('$\\beta_1'))


## --------------------------------------------------------
print(posterior, digits = 3)


## --------------------------------------------------------
post <- as.mcmc(posterior)
post_means <- apply(post, 2, mean)
post <- as.data.frame(post)


# Section 11.7 Bayesian Inferences with Simple Linear Regression

## --------------------------------------------------------
ggplot(PriceAreaData, aes(newsize, price)) +
  geom_point(size=3) +
  geom_abline(data=post[1:10, ],
              aes(intercept=beta0, slope=beta1),
              alpha = 0.5,
              color = crcblue) +
  geom_abline(intercept = post_means[1],
              slope = post_means[2],
              size = 2,
              color = crcblue) +
  ylab("Price") +
  xlab("Size") +
  theme_grey(base_size = 18, base_family = "")


## ----warning=FALSE---------------------------------------
size <- 1
mean_response <- post[, "beta0"] +
  size * post[, "beta1"]
post <- as.data.frame(post)
one_expected <- function(x){
  lp <- post[ , "beta0"] +  x * post[ , "beta1"]
  data.frame(Value = paste("Size =", x),
             Expected_Price = lp)
}
df <- map_df(c(1.2, 1.6, 2.0, 2.4), one_expected)


## ----warning=FALSE---------------------------------------
ggplot(df, aes(x = Expected_Price, y = Value)) +
  geom_density_ridges(fill = crcblue) +
  theme_grey(base_size = 18, base_family = "")


## ----warning=FALSE---------------------------------------
### summaries

df %>% group_by(Value) %>%
  summarize(P05 = quantile(Expected_Price, 0.05),
            P50 = median(Expected_Price),
            P95 = quantile(Expected_Price, 0.95))


## ----warning=FALSE---------------------------------------
one_predicted <- function(x){
  lp <- post[ , "beta0"] +  x * post[ , "beta1"]
  y <- rnorm(5000, lp, post[, "sigma"])
  data.frame(Value = paste("Size =", x),
             Predicted_Price = y)
}


## ----warning=FALSE---------------------------------------
set.seed(123)
df <- map_df(c(1.2, 1.6, 2.0, 2.4), one_predicted)

ggplot(df, aes(x = Predicted_Price, y = Value)) +
  geom_density_ridges(fill = crcblue) +
  theme_grey(base_size = 18, base_family = "")


## ----warning=FALSE---------------------------------------
### summaries

df %>% group_by(Value) %>%
  summarize(P05 = quantile(Predicted_Price, 0.05),
            P50 = median(Predicted_Price),
            P95 = quantile(Predicted_Price, 0.95))


## ----warning=FALSE---------------------------------------
#############################################
######## Simulating replicated datasets #####
#############################################
rep_data <- function(j){
  k <- sample(5000, size = 1)
  lp <- post[k , "beta0"] +
    PriceAreaData$newsize * post[ k, "beta1"]
  y <- rnorm(24, lp, post[k , "sigma"])
  data.frame(Type = paste("Simulation", j),
             size = PriceAreaData$newsize,
             price = y)
}


## ----warning=FALSE---------------------------------------
df <- map_df(1:8, rep_data)
df_obs <- select(PriceAreaData, newsize, price) %>%
  mutate(Type = "Observed",
         size = newsize) %>%
  select(Type, size, price)
rbind(df, df_obs) -> DF


## ----warning=FALSE---------------------------------------
ggplot(DF, aes(size, price)) +
  geom_point(size=2) +
  facet_wrap(~ Type, ncol = 3) +
  theme_grey(base_size = 18, base_family = "")


## --------------------------------------------------------
#############################################
######## Predictive residuals ###############
#############################################
one_p_residual <- function(x, y){
  lp <- post[ , "beta0"] +  x * post[ , "beta1"]
  yp <- rnorm(5000, lp, post[, "sigma"])
  residual <- y - yp
  s_residual <- quantile(residual, c(.05, .95))
  data.frame(Value = x,
             LO = s_residual[1],
             HI = s_residual[2])
}
df <- map2_df(x, y, one_p_residual)


## --------------------------------------------------------
ggplot(df, aes(x, LO)) +
  geom_linerange(aes(ymin = LO,
                     ymax = HI),
                 color = crcblue,
                 size = 1.3) +
  xlab("Size") +
  ylab("Predictive Residual") +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  theme_grey(base_size = 18, base_family = "")

# Section 11.8 Informative Prior

## ----warning=FALSE---------------------------------------
PriceAreaData$price_standardized <- scale(PriceAreaData$price)
PriceAreaData$size_standardized <- scale(PriceAreaData$newsize)


## ----warning=FALSE---------------------------------------
g2 <- ggplot(PriceAreaData, aes(x = size_standardized, y = price_standardized)) +
  geom_point(size=3) +
  xlab("Size (standardized)") +
  ylab("Price (standardized)") +
  theme(text=element_text(size=18))
g2
grid.arrange(g1, g2, ncol=1)


## ----warning=FALSE---------------------------------------
modelString <-"
model {

## sampling
for (i in 1:N){
y[i] ~ dnorm(beta0 + beta1*x[i], invsigma2)
}

## priors
beta0 ~ dnorm(mu0, g0)
beta1 ~ dnorm(mu1, g1)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}
"


## ----warning=FALSE---------------------------------------
PriceAreaData$price_standardized <- scale(PriceAreaData$price)
PriceAreaData$size_standardized <- scale(PriceAreaData$newsize)
y <- as.vector(PriceAreaData$price_standardized)
x <- as.vector(PriceAreaData$size_standardized)
N <- length(y)


## ----warning=FALSE---------------------------------------
the_data <- list("y" = y, "x" = x, "N" = N,
                 "mu0" = 0, "g0" = 1,
                 "mu1" = 0.7, "g1" = 44.4,
                 "a" = 1, "b" = 1)


## ----warning=FALSE---------------------------------------
initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## ----warning=FALSE---------------------------------------
posterior2 <- run.jags(modelString,
                       n.chains = 1,
                       data = the_data,
                       monitor = c("beta0", "beta1", "sigma"),
                       adapt = 1000,
                       burnin = 5000,
                       sample = 5000,
                       inits = initsfunction)


## ----warning=FALSE---------------------------------------
post2 <- as.mcmc(posterior2)

the_data <- list("y" = y, "x" = x, "N" = N,
                 "mu0" = 0, "g0" = 0.0001,
                 "mu1" = 0.7, "g1" = 0.0001,
                 "a" = 1, "b" = 1)

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## ----warning=FALSE---------------------------------------
posterior3 <- run.jags(modelString,
                       n.chains = 1,
                       data = the_data,
                       monitor = c("beta0", "beta1", "sigma"),
                       adapt = 1000,
                       burnin = 5000,
                       sample = 5000,
                       inits = initsfunction)


## ----warning=FALSE---------------------------------------
post3 <- as.mcmc(posterior3)

post2 %>%
  as.data.frame() %>%
  mutate(Type = "Informative") -> post2a

post3 %>%
  as.data.frame() %>%
  mutate(Type = "Vague") -> post3a

post23 <- rbind(post2a, post3a)


## ----warning=FALSE---------------------------------------
ggplot(post23, aes(beta1, linetype = Type)) +
  geom_density(size = 1.2, color = crcblue) +
  theme(legend.position = "none") +
  annotate(geom = "text",
           x = 0.45, y = 3,
           label = "Informative", size = 7) +
  annotate(geom = "text",
           x = 1.15, y = 1.5,
           label = "Vague", size = 7) +
  theme_grey(base_size = 18, base_family = "") +
  xlab(TeX('$\\beta_1'))


## ----warning=FALSE---------------------------------------
print(posterior2, digits = 3)
print(posterior3, digits = 3)


## ----warning=FALSE---------------------------------------
gas_prices <- read_csv("../data/gas2017.csv")

ggplot(gas_prices, aes(Temp, Bill)) +
  geom_point(size = 3) +
  xlab("Average Monthly Temperature (degrees F)") +
  ylab("Monthly Bill (dollars)") +
  theme(text = element_text(size=18))


## ----warning=FALSE---------------------------------------
modelString = "
model{
beta1 <- (mu2 - mu1) / (x2 - x1)
beta0 <- mu1 - x1 * (mu2 - mu1) / (x2 - x1)
mu1 ~ dnorm(m1, s1)
mu2 ~ dnorm(m2, s2)
}"


## ----warning=FALSE---------------------------------------
the_data <- list("x1" = 40,
                 "x2" = 60,
                 "m1" = 100,
                 "m2" = 50,
                 "s1" = 1 / 20 ^ 2,
                 "s2" = 1 / 15 ^ 2)


## ----warning=FALSE---------------------------------------
initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## ----warning=FALSE---------------------------------------
prior <- run.jags(modelString,
                       n.chains = 1,
                       data = the_data,
                       monitor = c("beta0", "beta1"),
                       adapt = 1000,
                       burnin = 5000,
                       sample = 5000,
                       inits = initsfunction)


## ----warning=FALSE---------------------------------------
prior %>% as.mcmc() %>% as.data.frame() -> prior_df
ggplot(prior_df, aes(beta0, beta1)) +
  geom_point() +
  theme(text=element_text(size=18))

