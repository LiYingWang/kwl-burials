#--------------------------Bayesian inference on ERGMs-------------------------
# follow Alberto Caimo et al. (2015) hospital example
# prior uses normal distribution (low density, high transitivity, low popularity)
# need to adjust according to the observed ERGM network
# priors below follow the order of variables (without#) specified in model 3 (lines 148-159)
pre_prior_mean <- c(-5, 0, 0, 3, 0, 3, -3, 0) # positive prior number for edge means high density
pre_prior_sigma <- diag(c(3, 5, 5, 3, 5, 3, 3, 5), 8, 8) # covariance matrix structure, uncertainty
# normal distribution 𝜃 ∼ Nd (𝜇prior , Σprior ) a common prior model
# where the dimension d corresponds to the number of parameters, 𝜇 is mean vector and Σprior is a d × d covariance matrix
# output includes estimated posterior means, medians and 95% credible intervals

pre_bergm <- bergm(model_pre_3, # using the approximate exchange algorithm
                   prior.mean  = pre_prior_mean,
                   prior.sigma = pre_prior_sigma,
                   burn.in     = 1000, # drop first 100 for every chain of the population
                   main.iters  = 30000, # iterations for every chain of the population
                   aux.iters   = 10000, # MCMC steps used for network simulation
                   nchains     = 32, # number of chains of the population MCMC
                   gamma       = 0) # scalar; parallel adaptive direction sampling move factor, acceptance rate, 0.2

summary(pre_bergm)
plot(pre_bergm)


# get the first set of diagnostic plots
z <- pre_bergm

png(filename = here::here("analysis", "figures", "pre-32chains-30000iters.png"),
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
