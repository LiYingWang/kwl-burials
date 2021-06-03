library(Bergm)

# model 3 considers cluster, degree, and node attributes
model_pre_3 <- burial_network_pre ~ edges +  # the overall density of the network
  #nodematch('quantity') + # quantity-based homophily, the similarity of connected nodes
  nodematch('age') +
  nodematch('sex') +
  nodematch('ritual_pottery') +
  nodematch('value_class') +
  #nodematch('orientation') +
  #absdiff('burial_value') + for numeric variable
  gwesp(0.4, fixed = TRUE) + # close to zero and move up to see how well it matches triangles
  #gwnsp(0.4, fixed = TRUE) + # opposite to gwesp
  gwdegree(0.5, fixed = TRUE) +
  dyadcov(pre_distance_n, "dist")
summary(model_pre_3)
#--------------------------Bayesian inference on ERGMs-------------------------
# follow Alberto Caimo et al. (2015) hospital example
# prior uses normal distribution (low density, high transitivity, low popularity)
# need to adjust according to the observed ERGM network
# priors below follow the order of variables (without#) specified in model 3 (lines 148-159)
pre_prior_mean <- c(-3, 0, 0, 3, 0, 3, -3, 0) # positive prior number for edge means high density
pre_prior_sigma <- diag(c(1, 5, 5, 1, 5, 1, 1, 5), 8, 8) # covariance matrix structure, uncertainty
# normal distribution 𝜃 ∼ Nd (𝜇prior , Σprior ) a common prior model
# where the dimension d corresponds to the number of parameters, 𝜇 is mean vector and Σprior is a d × d covariance matrix
# output includes estimated posterior means, medians and 95% credible intervals

pre_bergm <- bergm(model_pre_3, # using the approximate exchange algorithm
                   prior.mean  = pre_prior_mean,
                   prior.sigma = pre_prior_sigma,
                   burn.in     = 1000, # drop first 100 for every chain of the population
                   main.iters  = 30000, # iterations for every chain of the population
                   aux.iters   = 5000, # MCMC steps used for network simulation
                   nchains     = 32, # number of chains of the population MCMC
                   gamma       = 0) # scalar; parallel adaptive direction sampling move factor, acceptance rate, 0.2

summary(pre_bergm)
plot(pre_bergm)


# get the first set of diagnostic plots
z <- pre_bergm

png(filename = here::here("analysis", "figures", "pre-8chains-30000iters-5000aux.png"),
    width = 10, height = 12, units = "in", res = 360)

seqq <- 4
par(mfrow = c(z$dim, 3),
    oma   = c(0, 0, 3, 0),
    mar   = c(4, 3, 1.5, 1))

for (i in 1:z$dim) {

  plot(density(z$Theta[, i]),
       main = "",
       axes = FALSE,
       xlab = bquote(paste(theta[.(i)], " (", .(z$specs[i]), ")")),
       ylab = "", lwd = 2)
  axis(1)
  axis(2)
  coda::traceplot(z$Theta[, i], type = "l", xlab = "Iterations", ylab = "")
  coda::autocorr.plot(z$Theta[, i], auto.layout = FALSE)
  if (z$dim > 4) seqq <- seq(4, z$dim, 4)
}

dev.off()

#--------------------Bayesian inference for ERGMs-------------------------
model_post_3 <- burial_network_post ~
  edges +  # the overall density of the network
  #nodematch('quantity') +  # quantity-based homophily, the similarity of connected nodes
  nodematch('age') +
  nodematch('sex') +
  nodematch('ritual_pottery') +
  nodematch('value_class') +
  #nodematch('orientation') +
  #absdiff('burial_value') +
  gwesp(1.2, fixed = TRUE) + # start from zero and move up to match the count of triangles
  #gwnsp(1.2, fixed = TRUE) +
  gwdegree(0.5, fixed = TRUE) +
  dyadcov(post_distance_n, "dist")

summary(model_post_3)

# Specify a prior distribution
# normal distribution (low density, low transitivity, high popularity)
# priors below follow the order of variables (without#) specified in model 3 (lines 126-138)
post_prior_mean <- c(-3, 0, 0, 0, 3, 1, 3, 0) # prior mean corresponds to mean for each parameter
post_prior_sigma <- diag(c(1, 5, 5, 5, 1, 1, 1, 5), 8, 8) # covariance matrix structure

post_bergm <- bergm(model_post_3,
                    prior.mean  = post_prior_mean ,
                    prior.sigma = post_prior_sigma,
                    burn.in     = 1000, # burn-in iterations for every chain of the population, drops the first 200
                    main.iters  = 30000, # iterations for every chain of the population
                    aux.iters   = 5000, # MCMC steps used for network simulation
                    nchains     = 16, # number of chains of the population MCMC
                    gamma       = 0) # scalar; parallel adaptive direction sampling move factor, acceptance rate

summary(post_bergm)
plot(post_bergm)

# get the first set of diagnostic plots
z <- post_bergm

png(filename = here::here("analysis", "figures", "post-16chains-30000iters-5000aux-with-coin.png"),
    width = 10, height = 12, units = "in", res = 360)

seqq <- 4
par(mfrow = c(z$dim, 3),
    oma   = c(0, 0, 3, 0),
    mar   = c(4, 3, 1.5, 1))

for (i in 1:z$dim) {

  plot(density(z$Theta[, i]),
       main = "",
       axes = FALSE,
       xlab = bquote(paste(theta[.(i)], " (", .(z$specs[i]), ")")),
       ylab = "", lwd = 2)
  axis(1)
  axis(2)
  coda::traceplot(z$Theta[, i], type = "l", xlab = "Iterations", ylab = "")
  coda::autocorr.plot(z$Theta[, i], auto.layout = FALSE)
  if (z$dim > 4) seqq <- seq(4, z$dim, 4)
}

dev.off()

png(filename = here::here("analysis", "figures", "post-bgof-test.png"),
    width = 5, height = 8, units = "in", res = 360)

bgof_post_test <-
  bgof2(post_bergm,
       sample.size = 10,
       aux.iters = 100,
       n.deg     = 30,
       n.dist    = 15,
       n.esp     = 30)

dev.off()
