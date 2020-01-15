mlm_regression <- function(d1){
  require(runjags)
  require(tidyverse)
  require(coda)
modelString = "
model {
for (i in 1:N){
y[i] ~ dbin(p[i], n[i])
logit(p[i]) <- a[player[i]] + b[player[i]] * (x[i] - 30) +
            c[player[i]] * (x[i] - 30) * (x[i] - 30)
}
for (j in 1:J){
a[j] <- B[j,1]
b[j] <- B[j,2]
c[j] <- B[j,3]
B[j,1:3] ~ dmnorm (mu.beta[], Tau.B[,])
}
mu.beta[1:3] ~ dmnorm(mean[1:3],prec[1:3 ,1:3 ])
Tau.B[1:3 , 1:3] ~ dwish(Omega[1:3 ,1:3 ], 3)
}
"
# data for JAGS

b.data <- list(y = d1$OB,
               x = d1$Age,
               player = d1$Player,
               n = d1$PA,
               N = dim(d1)[1],
               J = max(d1$Player),
               mean = c(0, 0, 0),
               Omega = diag(c(.1,.1,.1)),
               prec = diag(c(1.0E-6,1.0E-6,1.0E-6)))

# implement MCMC

posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = b.data,
                      monitor = c("B", "mu.beta"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 20000)

post <- as.mcmc(posterior)
post <- as.data.frame(post)

# compute posterior means, use these to get estimated
# probabilities

est <- apply(post, 2, mean)[1:(3 * b.data$J)]
est <- as.data.frame(matrix(est, b.data$J, 3, byrow = FALSE))
names(est) <- c("b0", "b1", "b2")
est$Player <- 1:b.data$J
d1 <- inner_join(d1, est)
d1 %>% mutate(lp = b0 + b1 * (Age - 30) +
                b2 * (Age - 30) ^ 2,
              p = exp(lp) / (1 + exp(lp)))  -> d1

# plot 2nd stage fit

one_fit <- function(ages){
  j <- sample(dim(post)[1], size = 1)
  lp <- post[j, "mu.beta[1]"] + (ages - 30) * post[j, "mu.beta[2]"] +
    (ages - 30) ^ 2 * post[j, "mu.beta[3]"]
  exp(lp) / (1 + exp(lp))
}
ages <- seq(22, 38, by = 0.5)
thedata <- NULL
for(j in 1:50){
  d <- data.frame(Iteration = j, Age = ages, p = one_fit(ages))
  thedata <- rbind(thedata, d)
}
plot2 <- ggplot(thedata, aes(Age, p)) +
  geom_line(aes(group = Iteration),
            color = "blue")

list(d = d1, plot2 = plot2)

}
