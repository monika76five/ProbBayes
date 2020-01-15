
library(tidyverse)
require(gridExtra)
require(runjags)
require(coda)
library(ggridges)
library(ProbBayes)
library(Lahman)
library(ggrepel)
crcblue <- "#2905a1"

# Section 13.2 Federalist Papers Study

## --------------------------------------------------------
## read in data -- Hamilton, "can" frequencies

d1 <- read_csv("../data/Hamilton_can.csv")
d2 <- read_csv("../data/Madison_can.csv")


## --------------------------------------------------------
ggplot(rbind(d1, d2), aes(Authorship, Rate)) +
  geom_jitter(width = 0.1, size = 3) +
  coord_flip() +
  theme(text=element_text(size=18))



## --------------------------------------------------------
modelString = "
model{
## sampling
for (i in 1:N) {
y[i] ~ dpois(n[i] * lambda / 1000)
}

## prior
lambda ~ dgamma(0.001, 0.001)
}
"


## --------------------------------------------------------
y <- d1$N
n <- d1$Total
the_data <- list("y" = y, "n" = n, N = length(y))


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
                 monitor = c("lambda"),
                 n.chains = 1,
                 burnin = 2000,
                 sample = 5000,
                 inits = initsfunction)


## --------------------------------------------------------
post <- as.mcmc(posterior)


## --------------------------------------------------------
one_rep <- function(i){
  lambda <- post[i]
  sd(rpois(length(y), n * lambda / 1000))
}
sapply(1:5000, one_rep) -> SD

ggplot(data.frame(sd = SD), aes(sd)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) + theme(text=element_text(size=18)) +
  geom_vline(xintercept = sd(y), size = 3 ) +
  annotate('text', x = 4, y = 950,
           label = "Observed", size = 7)


## --------------------------------------------------------
prob1 <- mean(SD >= sd(y))
prob1


## --------------------------------------------------------
modelString = "
model{
## sampling
for(i in 1:N){
p[i] <- beta / (beta + n[i] / 1000)
y[i] ~ dnegbin(p[i], alpha)
}

## priors
mu <- alpha / beta
alpha ~ dgamma(.001, .001)
beta ~ dgamma(.001, .001)
}
"


## --------------------------------------------------------
y <- d1$N
n <- d1$Total
the_data <- list("y" = y, "n" = n, N = length(y))


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
                 monitor = c("alpha", "beta", "mu"),
                 n.chains = 1,
                 burnin = 2000,
                 sample = 5000,
                 inits = initsfunction)


## --------------------------------------------------------
post <- as.data.frame(as.mcmc(posterior))


## --------------------------------------------------------
one_rep <- function(i){
  p <- post$beta[i] / (post$beta[i] + n / 1000)
  sd(rnbinom(length(y), size = post$alpha[i], prob = p))
}
sapply(1:5000, one_rep) -> SD

# Figure 13.3

ggplot(data.frame(sd = SD), aes(sd)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) + theme(text=element_text(size=18)) +
  geom_vline(xintercept = sd(y), size = 3 ) +
  annotate('text', x = 7, y = 1250,
           label = "Observed", size = 7)


## --------------------------------------------------------
plot(posterior, vars = "mu")


## --------------------------------------------------------
print(posterior, digits = 4)


## --------------------------------------------------------
modelString = "
model{
## sampling
for(i in 1:N1){
p1[i] <- beta1 / (beta1 + n1[i] / 1000)
y1[i] ~ dnegbin(p1[i], alpha1)
}
for(i in 1:N2){
p2[i] <- beta2 / (beta2 + n2[i] / 1000)
y2[i] ~ dnegbin(p2[i], alpha2)
}

## priors
alpha1 ~ dgamma(.001, .001)
beta1 ~ dgamma(.001, .001)
alpha2 ~ dgamma(.001, .001)
beta2 ~ dgamma(.001, .001)
ratio <- (alpha2 / beta2) / (alpha1 / beta1)
}
"


## --------------------------------------------------------
d1 <- read_csv("../data/Hamilton_can.csv")
d2 <- read_csv("../data/Madison_can.csv")

the_data <- list("y1" = d1$N, "n1" = d1$Total,
                 "N1" = length(d1$N),
                 "y2" = d2$N, "n2" = d2$Total,
                 "N2" = length(d2$N))


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
                 monitor = c("ratio"),
                 n.chains = 1,
                 burnin = 2000,
                 sample = 5000,
                 inits = initsfunction)


## --------------------------------------------------------
plot(posterior)


## --------------------------------------------------------
quantile(as.mcmc(posterior$mcmc), c(.05, .5, .95))


## --------------------------------------------------------
d <- read_csv("../data/federalist_word_study.csv")


## --------------------------------------------------------
modelString = "
model{
for(i in 1:N1){
p1[i] <- beta1 / (beta1 + n1[i] / 1000)
y1[i] ~ dnegbin(p1[i], alpha1)
}
for(i in 1:N2){
p2[i] <- beta2 / (beta2 + n2[i] / 1000)
y2[i] ~ dnegbin(p2[i], alpha2)
}
alpha1 ~ dgamma(.001, .001)
beta1 ~ dgamma(.001, .001)
alpha2 ~ dgamma(.001, .001)
beta2 ~ dgamma(.001, .001)
ratio <- (alpha2 / beta2) / (alpha1 / beta1)
}
"


## --------------------------------------------------------
bayes_one_word <- function(theword){
  d1 <- filter(d, Authorship == "Hamilton",
               word == theword)
  d2 <- filter(d, Authorship == "Madison",
               word == theword)
  the_data <- list("y1" = d1$N,
                   "n1" = d1$Total,
                   "N1" = length(d1$N),
                   "y2" = d2$N,
                   "n2" = d2$Total,
                   "N2" = length(d2$N))
posterior <- run.jags(modelString,
                   data = the_data,
                   monitor = c("ratio"),
                   n.chains = 1,
                   burnin = 2000,
                   sample = 5000,
                   inits = initsfunction)
  quantile(as.mcmc(posterior$mcmc), c(.025, .5, .975))
}


## --------------------------------------------------------
word_list <- c("by", "from", "to", "an", "any", "may",
               "his", "upon", "also", "can",
               "of", "on", "there", "this")
word_list <- sort(word_list)
S <- sapply(word_list, bayes_one_word)

df <- data.frame(Word = word_list,
                 LO = S[1, ],
                 M = S[2, ],
                 HI = S[3, ])


## --------------------------------------------------------
ggplot(df, aes(x = Word, y = M)) +
  geom_errorbar(aes(ymin = LO, ymax = HI), width = 0.3, size = 1) +
  geom_point() + coord_flip() +
  theme(text=element_text(size=18))  +
  ylab("Ratio") +
  geom_hline(yintercept = 1)


# Section 13.3 Career Trajectories

# Read in three local functions
## --------------------------------------------------------
source("get_onbase_data.R")
source("compute_individual_regressions.R")
source("mlm_regression.R")

## --------------------------------------------------------
# work with players born in 1978 with
# at least 1000 career at-bats

d78 <- get_onbase_data(1978, 1000)

# fit individual regressions

d78_b <- compute_individual_regressions(d78)

## --------------------------------------------------------
ggplot(filter(d78_b,
              nameLast == "Utley",
              OB >= 20),
       aes(Age, OB / PA)) +
  geom_point(size = 3) +
  geom_line(aes(Age, P), size=1.2) +
  theme(text=element_text(size=18))


## --------------------------------------------------------
ggplot(filter(d78_b,
              nameLast == 'Phelps',
              OB >= 20),
       aes(Age, OB / PA)) +
  geom_point(size = 3) +
  geom_line(aes(Age, P), size=1.2) +
  increasefont()


## --------------------------------------------------------
# Individual logistic fit

modelString = "
model {
## sampling
for (j in 1:N){
y[j] ~ dbin(p[j], n[j])
logit(p[j]) <- beta0 + beta1 * (x[j] - 30) +
            beta2 * (x[j] - 30) * (x[j] - 30)
}

## priors
beta0 ~ dnorm(0, 0.0001)
beta1 ~ dnorm(0, 0.0001)
beta2 ~ dnorm(0, 0.0001)
}
"


## --------------------------------------------------------
utley <- filter(d78, nameLast == "Utley")

the_data <- list(y = utley$OB,
               x = utley$Age,
               n = utley$PA,
               N = dim(utley)[1])


## --------------------------------------------------------
# initialization function

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## --------------------------------------------------------
# implement MCMC

posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta0", "beta1", "beta2"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 20000,
                      inits = initsfunction)


## --------------------------------------------------------
post <- as.mcmc(posterior)
post <- as.data.frame(post)


## --------------------------------------------------------
invlogit <- function(x) exp(x) / (1 + exp(x))

post %>%
  mutate(peak.age = 30 - beta1  / 2 / beta2,
         peak = invlogit(beta0 - beta1 ^ 2 / 4/ beta2)) -> post


## --------------------------------------------------------
d_one <- data.frame(Type = "Peak Age",
                    Value = post$peak.age)

d_one <- filter(d_one, Value > 26, Value < 40)

d_two <- data.frame(Type = "Peak",
                    Value = post$peak)

d_two <- filter(d_two, Value < .44)

d_all <- rbind(d_one, d_two)


## --------------------------------------------------------
# Figure 12.9

ggplot(d_all, aes(Value)) +
  geom_density() +
  facet_wrap(~ Type, scale = "free", ncol = 1) +
  increasefont()


## --------------------------------------------------------
# Figure 12.10

ggplot(d78_b) +
  geom_point(aes(Age, OB / PA)) +
  facet_wrap(~ nameLast) +
  ylim(.25, .44) +
  geom_line(aes(Age, P)) +
  increasefont()


## --------------------------------------------------------
# Figure 12.11

out <- mlm_regression(d78_b)

out$plot2 + ylim(0, .35) +
  increasefont()


## --------------------------------------------------------
# Figure 12.12

ggplot(filter(out$d, nameLast %in%
                c("Utley", "Phelps")),
       aes(Age, OB / PA)) +
  geom_point(size = 3) +
  geom_line(aes(Age, P), size=1.2) +
  geom_line(aes(Age, p), size=1.2, linetype = "dashed") +
  increasefont() +
  ylim(.25, .45) +
  facet_wrap(~ nameLast, ncol = 1)

# Section 13.4 Latent Class Modeling

## --------------------------------------------------------
ScoreData <- read_csv("../data/ScoreData.csv")


## --------------------------------------------------------
ggplot(ScoreData, aes(x=Person, y=Score)) +
  geom_point(size=3) +
  ylim(5, 20) +
  labs(x = "Person Index", y = "Score") +
  theme_grey(base_size = 30, base_family = "") +
  geom_text_repel(aes(label=Person), size = 10)


## --------------------------------------------------------
## write the model
modelString<-"
model {
## sampling
for (i in 1:N){
theta[i] <- equals(z[i], 1) * p1 + equals(z[i], 0) * p0
y[i] ~ dbin(theta[i], m)
}
for (i in 1:N){
z[i] ~ dbern(1/3)
}

## priors
p1 <- 0.5
p0 ~ dbeta(1,1) T(0.5, 1)
}
"


## --------------------------------------------------------
y <- ScoreData$Score
N <- length(y)


## --------------------------------------------------------
initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## --------------------------------------------------------
the_data <- list("y" = y, "N" = N, "m" = 20)


## --------------------------------------------------------
posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("z", "p0", "theta"),
                      adapt = 1000,
                      burnin = 20000,
                      sample = 5000,
                      thin = 5,
                      inits = initsfunction)


## --------------------------------------------------------
summary(posterior)


## --------------------------------------------------------
plot(posterior, vars = "p0")


## --------------------------------------------------------
### Mixture model - Scenario 2
## write the model
modelString<-"
model {
## sampling
for (i in 1:N){
theta[i] <- equals(z[i], 1) * p1 + equals(z[i], 0) * p0
y[i] ~ dbin(theta[i], m)
}
for (i in 1:N){
z[i] ~ dbern(q)
}

## priors
p1 ~ dbeta(1, 1) T(0.4, 0.6)
p0 ~ dbeta(1,1) T(p1, 1)
q ~ dbeta(1, 1)
}
"


## --------------------------------------------------------
y <- ScoreData$Score
N <- length(y)

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}


## --------------------------------------------------------
the_data <- list("y" = y, "N" = N, "m" = 20)


## --------------------------------------------------------
posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("z", "p1", "p0", "q"),
                      adapt = 1000,
                      burnin = 20000,
                      sample = 5000,
                      thin = 5,
                      inits = initsfunction)


## --------------------------------------------------------
post <- as.mcmc(posterior)


## --------------------------------------------------------
post_means  <- data.frame(Index = 1:30,
                          Mean = apply(post, 2, mean)[1:30])


## --------------------------------------------------------
ggplot(post_means, aes(Index, Mean)) +
  geom_point(size = 3) +
  theme_grey(base_size = 20, base_family = "")


## --------------------------------------------------------
ggplot(post_means, aes(x=Index, y=Mean)) +
  geom_point(size=3) +
  ylim(0, 1) +
  labs(x = "Person Index", y = "Mean") +
  theme_grey(base_size = 30, base_family = "") +
  geom_text_repel(aes(label=Index), size = 10)


## --------------------------------------------------------
post <- as.data.frame(post)
post %>% select(p1, p0, q) %>%
  gather(Type, Value) -> post2


## --------------------------------------------------------
ggplot(post2, aes(x = Value, y = Type)) +
  geom_density_ridges() +
  theme_grey(base_size = 20, base_family = "")


## --------------------------------------------------------
summary(posterior)

