#
library(ggplot2)
library(gridExtra)
library(ProbBayes)
library(VGAM)
library(tidyverse)
library(coda)
library(reshape2)
library(ggridges)
library(runjags)
#library(CalledStrike)
crcblue <- "#2905a1"


# Section 10.2 Hierarchical Normal Modeling

## --------------------------------------------------------
MovieRatings = read.csv("../data/2010_animation_ratings.csv", header = TRUE, sep = ",")


## --------------------------------------------------------
table(MovieRatings$Group_Number)


## --------------------------------------------------------
## recode the movie titles

MovieRatings %>%
  mutate(Title = as.character(title),
         Title = recode(Title,
                  "Shrek Forever After (a.k.a. Shrek: The Final Chapter) (2010)" = "Shrek Forever",
                  "How to Train Your Dragon (2010)" = "Dragon",
                  "Toy Story 3 (2010)" = "Toy Story 3",
                  "Tangled (2010)" = "Tangled",
                  "Despicable Me (2010)" = "Despicable Me",
                  "Legend of the Guardians: The Owls of Ga'Hoole (2010)" = "Guardians",
                  "Megamind (2010)" = "Megamind",
                  "Batman: Under the Red Hood (2010)" = "Batman")) ->
           MovieRatings


## --------------------------------------------------------
ggplot(MovieRatings, aes(Title, rating)) +
  geom_jitter(width = 0.2,
              size = 3, color = crcblue) +
  coord_flip() +
  theme(text=element_text(size=18)) +
  ylab("Rating")


## --------------------------------------------------------
modelString <-"
model {
## sampling
for (i in 1:N){
y[i] ~ dnorm(mu_j[MovieIndex[i]], invsigma2)
}
## priors
for (j in 1:J){
mu_j[j] ~ dnorm(mu, invtau2)
}
invsigma2 ~ dgamma(a_s, b_s)
sigma <- sqrt(pow(invsigma2, -1))
## hyperpriors
mu ~ dnorm(mu0, g0)
invtau2 ~ dgamma(a_t, b_t)
tau <- sqrt(pow(invtau2, -1))
}
"


## --------------------------------------------------------
y = MovieRatings$rating        # The y values are in the column named rating
MovieIndex = MovieRatings$Group_Number      # The MovieIndex is in the column named Group_Number
N = length(y)  # Compute the number of observations
J = length(unique(MovieIndex)) # Compute the number of movies

the_data <- list("y" = y, "MovieIndex" = MovieIndex,
                 "N" = N, "J" = J,
                 "mu0" = 3, "g0" = 1,
                 "a_t" = 1, "b_t" = 1,
                 "a_s" = 1, "b_s" = 1)


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
            monitor = c("mu", "tau", "mu_j", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      inits = initsfunction)


## --------------------------------------------------------
plot(posterior, vars = "tau")


## --------------------------------------------------------
print(posterior, digits = 3)


## --------------------------------------------------------
Post_Means <- summary(posterior)[, 4]


## --------------------------------------------------------
MovieRatings %>% group_by(Group_Number) %>%
  summarize(Title = first(title),
            N = n(), M = mean(rating),
            SE = sd(rating) / sqrt(N)) -> Ind_Stats


## --------------------------------------------------------
Means1 <- data.frame(Type = "Sample", Mean = Ind_Stats$M)
Means2 <- data.frame(Type = "Posterior", Mean =
                       Post_Means[3:(4 + J - 2)])
Means1$Title <- c("Dragon", "Toy Story 3", "Shrek Forever",
                  "Despicable Me", "Batman", "Guardians",
                  "Megamind", "Tangled")
Means2$Title <- c("Dragon", "Toy Story 3", "Shrek Forever",
                  "Despicable Me", "Batman", "Guardians",
                  "Megamind", "Tangled")


## --------------------------------------------------------
ggplot(rbind(Means1, Means2),
       aes(Type, Mean, group=Title)) +
  geom_line(color = crcblue) + geom_point() +
  annotate(geom = "text",
           x = 0.75,
           y = Means1$Mean + c(0.05, 0, 0.05, 0,
                               0, -0.05, 0, 0),
           size = 6,
           label = Means1$Title) +
  theme(text=element_text(size=18))


## --------------------------------------------------------
tau_draws <- as.mcmc(posterior, vars = "tau")
sigma_draws <- as.mcmc(posterior, vars = "sigma")

R = tau_draws^2/(tau_draws^2 + sigma_draws^2)
summary(R)


## --------------------------------------------------------
df = as.data.frame(R)
ggplot(df, aes(x=R)) +
  geom_density(size = 1, color = crcblue) +
  theme(text=element_text(size=18))

# Section 10.3 Hierarchical Beta-Binomial Modeling

## --------------------------------------------------------
deathdata <- read.csv("../data/DeathHeartAttackDataNYCManhattan.csv", header=T)


## --------------------------------------------------------
deathdata$DeathRatio <- deathdata$Deaths / deathdata$Cases * 100


## --------------------------------------------------------
modelString <-"
model {

## likelihood
for (i in 1:N){
y[i] ~ dbin(p[i], n[i])
}

## priors
for (i in 1:N){
p[i] ~ dbeta(a, b)
}

## hyperpriors
a <- mu*eta
b <- (1-mu)*eta
mu ~ dbeta(mua, mub)
eta <- exp(logeta)
logeta ~ dlogis(logn, 1)
}
"


## --------------------------------------------------------
y = deathdata$Deaths        # The y values are in the column named Deaths
n = deathdata$Cases         # The n values are in the column named Cases
N = length(y)  # Compute the number of observations
the_data <- list("y" = y, "n" = n, "N" = N,
                 "mua" = 1, "mub" = 1,
                 "logn" = log(100))


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
                      monitor = c("p", "mu", "logeta"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      inits = initsfunction)


## --------------------------------------------------------
plot(posterior, vars = "logeta")


## --------------------------------------------------------
print(posterior, digits = 3)


## --------------------------------------------------------
Post_Means <- summary(posterior)[, 4]


## --------------------------------------------------------
Means1 <- data.frame(Type = "Sample",
                     Mean = deathdata$DeathRatio/100)
Means2 <- data.frame(Type = "Posterior", Mean =
                       Post_Means[1:13])


## --------------------------------------------------------
Means1$Hospital <- 1:13
Means2$Hospital <- 1:13
ggplot(rbind(Means1, Means2),
       aes(Type, Mean, group = Hospital)) +
  geom_line(color = crcblue) +
  geom_point(size = 3) +
  theme(text=element_text(size=18)) +
  ylab("Proportion") +
  annotate(geom = "text",
           x = 0.75, y = .125,
           label = "NYP - Allen", size = 6) +
  annotate(geom = "text",
           x = 0.75, y = .135,
           label = "Mt. Sinai Roos.", size = 6)


## --------------------------------------------------------
p_draws <- as.mcmc(posterior, vars = "p")

df = as.data.frame(cbind(seq(1:5000),p_draws))

df_long = melt(df, id.vars = "V1")

ggplot(df_long, aes(x = value, y = variable)) +
  geom_density_ridges(color = crcblue) +
  theme_grey(base_size = 20, base_family = "") +
  xlim(0, .15)


## --------------------------------------------------------
p11draws <- as.mcmc(posterior, vars = "p[11]")
p12draws <- as.mcmc(posterior, vars = "p[12]")
diff <- p11draws - p12draws
sum(diff > 0)/5000


## --------------------------------------------------------
df_long %>%
  group_by(V1) %>%
  mutate(Ranks = rank(value)) -> rank_data
rank_data %>%
  filter(variable == "p[1]")  %>%
  group_by(Ranks) %>%
  summarize(Count = n(),
            Proportion = Count / 5000)  %>%
  mutate(Type = "Bellevue Hospital Center") -> R1
rank_data %>%
  filter(variable == "p[2]")  %>%
  group_by(Ranks) %>%
  summarize(Count = n(),
            Proportion = Count / 5000)  %>%
  mutate(Type = "Harlem Hospital Center") -> R2


## --------------------------------------------------------
ggplot(rbind(R1, R2), aes(Ranks, Proportion)) +
  geom_col(fill = crcblue) +
  facet_wrap(~ Type, ncol = 1) +
  theme(text=element_text(size=18)) +
  xlab("Rank") + ylab("Probability")


## --------------------------------------------------------
rank_data %>%
  filter(Ranks == 1) -> rank1
rank1 %>% group_by(variable) %>%
  summarize(N = n()) %>%
        mutate(Probability = N / sum(N),
               Hospital =
                 deathdata$Hospital) -> S_rank1

S_rank1$Hosp <- c("Bellevue", "Harlem", "Lenox Hill",
                  "Metropolitan",
                  "Mount Sinai Beth Israel",
                  "Mount Sinai",
                  "Mount Sinai Rossevelt",
                  "Mount Sinai St. Luke's",
                  "NYU", "NYP - Allen",
                  "NYP - Columbia",
                  "NYP - NY Weill Corner",
                  "NYP - Lower Manhattan")


## --------------------------------------------------------
ggplot(S_rank1,
       aes(reorder(Hosp, Probability), Probability)) +
  geom_point(size = 4, color = crcblue) +
  coord_flip() +
  theme(text=element_text(size=18)) +
  xlab("Hospital")


## --------------------------------------------------------
m_rates <- read_csv("../data/marriage_counts.csv")

ggplot(m_rates, aes(Year, Count)) +
  geom_point(size = 4, color = crcblue) +
  theme(text=element_text(size=18)) +
  geom_vline(xintercept = c(1940.5, 1944.5),
             size = 1.5)

